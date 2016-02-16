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

require(xgboost); require(Matrix); require(data.table)

### Set up xgboost CV Train / Test Matrices #######
for (i in 1:folds){
  assign(paste0("dtrn",i), xgb.DMatrix(as.matrix(get(paste0("trn",i))),
                                       label = log(as.matrix(get(paste0("trnCost",i)))+1)))
  assign(paste0("dtst",i), xgb.DMatrix(as.matrix(get(paste0("tst",i)))))
}

### Specify xgboost Parameters ####################
params <- list(booster = "gbtree",
               eta = .1, # was .05
               min_child_weight = 1, # was 1
               subsample = .8, # was .8
               colsample_bytree = .8, # was .8
               max_depth = 6, # was 7
               objective = "reg:linear", # was reg:linear
               base_score = .5) # was base_score
numRounds = 3000

### Initiate Vector for OOB Predictions ###########
xgbCVPreds <- matrix(nrow = nrow(train), ncol = 1)
xgbCVPreds[,] <- 0

### Build CV Models and Assign Predictions ########
for (i in 1:folds){
  set.seed(900+i)
  assign(paste0("xgbMod",i), xgboost(params = params,
                                     data = get(paste0("dtrn",i)),
                                     nrounds = numRounds,
                                     verbose = 1, print.every.n = 100))
  xgbCVPreds[get(paste0("tstID",i))] <- exp(predict(get(paste0("xgbMod",i)),
                                                    get(paste0("dtst",i)))) - 1
}

### Write CV OOB Predictions to csv ################
write.csv(xgbCVPreds, file = "cat_xgb_trainPREDS.csv")

### Get Averaged Test Set Predictions ##############
dTest <- xgb.DMatrix(as.matrix(gtest))
for (i in 1:folds) {
  assign(paste0("P",i), predict(get(paste0("xgbMod",i)), dTest))
}

P = matrix(nrow = nrow(test), ncol = 1)
P[,] = 0
for (i in 1:folds) {P = P + get(paste0("P",i))}
xgbTestPreds = exp(P / folds) - 1

write.csv(xgbTestPreds, file = "cat_xgb_testPREDS.csv")