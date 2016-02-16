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

gtrain$tube_assembly_id = NULL; gtrain$supplier = NULL
gtest$tube_assembly_id = NULL; gtest$supplier = NULL
gtrain$bracket_pricing = NULL; gtest$bracket_pricing = NULL
gtrain$tubeMatl = NULL; gtest$tubeMatl = NULL
gtrain$endA = NULL; gtest$endA = NULL
gtrain$endX = NULL; gtest$endX = NULL
gtrain$endA1X = NULL; gtest$endA1X = NULL
gtrain$endA2X = NULL; gtest$endA2X = NULL
gtrain$endX1X = NULL; gtest$endX1X = NULL
gtrain$endX2X = NULL; gtest$endX2X = NULL
testID <- gtest$id
gtest$id = NULL

svmTrainPreds <- read.csv("cat_svm_trainPREDS.csv")[,2]
rfTrainPreds <- read.csv("cat_rf_trainPREDS.csv")[,2]
xgbTrainPreds <- read.csv("cat_xgb_trainPreds.csv")[,2]
svmTestPreds <- read.csv("cat_svm_testPREDS.csv")[,2]
rfTestPreds <- read.csv("cat_rf_testPREDS.csv")[,2]
xgbTestPreds <- read.csv("cat_xgb_testPreds.csv")[,2]

trainStack = cbind(gtrain, svmTrainPreds, rfTrainPreds, xgbTrainPreds)
testStack = cbind(gtest, svmTestPreds, rfTestPreds, xgbTestPreds)

trnStack = cbind(trainStack[,1:15],trainStack[,78:80])
tstStack = cbind(testStack[,1:15],testStack[,78:80])
colnames(tstStack) <- colnames(trnStack)

require(randomForest)
rfMod <- randomForest(trnStack, y = log(cost+1), ntree = 100)
rfPred <- exp(predict(rfMod, tstStack)) - 1

cat_combine_2 <- read.csv("cat_combine_2.csv")[,2]

out2 <- data.frame(id=as.integer(rownames(test)), cost = .8*cat_combine_2 + .2*rfPred)
write.csv(out2, file = "cat_stack.csv", quote = F, row.names = F)