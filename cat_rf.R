######################################################
### Kaggle - CAT Tube Quoting ########################
######################################################

### Set Working Directory ############################
setwd("C:/Projects/catCleanup/")

### Set up Parallel Solver ###########################
require(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

datadir <- "competition_data"
test <- read.csv(paste0(datadir,"/test_set.csv"), as.is = T)
train <- read.csv(paste0(datadir,"/train_set.csv"), as.is = T)
cost <- train$cost
train2 <- read.csv(file = "cat_train2.csv"); train2 <- train2[,2:ncol(train2)]
test2  <- read.csv(file = "cat_test2.csv"); test2 <- test2[,2:ncol(test2)]
idTest <- test$id

### Initiate Data Frames for Numeric Sets ##########
gtrain <- train2
gtest <- test2

### Remove Categorical Features ####################
gtrain$tube_assembly_id = NULL; gtrain$supplier = NULL
gtrain$bracket_pricing = NULL; gtrain$tubeMatl = NULL
gtrain$endA = NULL; gtrain$endX = NULL
gtrain$endA1X = NULL; gtrain$endA2X = NULL
gtrain$endX1X = NULL; gtrain$endX2X = NULL
gtest$tube_assembly_id = NULL; gtest$supplier = NULL
gtest$bracket_pricing = NULL; gtest$tubeMatl = NULL
gtest$endA = NULL; gtest$endX = NULL
gtest$endA1X = NULL; gtest$endA2X = NULL
gtest$endX1X = NULL; gtest$endX2X = NULL

### Create CV Folds ################################
require(caret)
folds = 10
set.seed(1301)
cvFolds <- createFolds(cost, k = folds, list = T, returnTrain = F)
for (i in 1:folds){
  assign(paste0("trn",i), gtrain[-cvFolds[[i]],])
  assign(paste0("tst",i), gtrain[ cvFolds[[i]],])
  assign(paste0("tstID",i), cvFolds[[i]])
  assign(paste0("trnCost",i), cost[-cvFolds[[i]]])
  assign(paste0("tstCost",i), cost[ cvFolds[[i]]])
}

### Initiate Vector for OOB Predictions ###########
rfCVPreds <- matrix(nrow = nrow(train), ncol = 1)
rfCVPreds[,] <- 0

### Build CV Models and Assign Predictions ########
require(randomForest)
trees = 300
for (i in 1:folds){
  set.seed(100+i)
  assign(paste0("rfMod",i), randomForest(get(paste0("trn",i)),
                                         y = log(get(paste0("trnCost",i))+1),
                                         ntree = trees))
  rfCVPreds[get(paste0("tstID",i))] <- exp(predict(get(paste0("rfMod",i)),
                                                    get(paste0("tst",i)))) - 1
}

### Write CV OOB Predictions to csv ################
write.csv(rfCVPreds, file = "cat_rf_trainPREDS.csv")

### Get Averaged Test Set Predictions ##############
for (i in 1:folds) {
  assign(paste0("P",i), predict(get(paste0("rfMod",i)), gtest))
}

P = matrix(nrow = nrow(test), ncol = 1)
P[,] = 0
for (i in 1:folds) {P = P + get(paste0("P",i))}
rfTestPreds = exp(P / folds) - 1

write.csv(rfTestPreds, file = "cat_rf_testPREDS.csv")