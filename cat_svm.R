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
folds = 20
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
svmCVPreds <- matrix(nrow = nrow(train), ncol = 1)
svmCVPreds[,] <- 0

### Build CV Models and Assign Predictions ########
require(e1071)
for (i in 1:folds){
  set.seed(200+i)
  assign(paste0("svmMod",i), svm(as.matrix(get(paste0("trn",i)))[,1:14],
                                 y = log(get(paste0("trnCost",i))+1),
                                 type = 'eps-regression', kernel = 'radial'))
  svmCVPreds[get(paste0("tstID",i))] <- exp(predict(get(paste0("svmMod",i)),
                                                    as.matrix(get(paste0("tst",i)))[,1:14])) - 1
}

### Write CV OOB Predictions to csv ################
write.csv(svmCVPreds, file = "cat_svm_trainPREDS.csv")

for (i in 1:folds) {
  assign(paste0("P",i), predict(get(paste0("svmMod",i)), as.matrix(gtest)[,1:14]))
}

P = matrix(nrow = nrow(test), ncol = 1)
P[,] = 0
for (i in 1:folds) {P = P + get(paste0("P",i))}
svmTestPreds = exp(P / folds) - 1

write.csv(svmTestPreds, file = "cat_svm_testPREDS.csv")

############################################
### ARCHIVED MODEL TUNING & TESTING ########
############################################

# svmModTest1 <- ksvm(as.matrix(trn1), y = log(trnCost1+1), scaled = FALSE, kpar = list(sigma = .000001),
#                     type = "eps-svr", kernel = "rbfdot")
# svmPredTest1 <- exp(predict(svmModTest1, as.matrix(tst1)))
# rmsleTest1 <- rmsle(tstCost1, svmPredTest1)
# print(paste("rmsleTest1", rmsleTest1))
# 
# svmModTest2 <- ksvm(as.matrix(trn1), y = log(trnCost1+1), scaled = FALSE, kpar = list(sigma = .001),
#                     type = "eps-svr", kernel = "rbfdot")
# svmPredTest2 <- exp(predict(svmModTest2, as.matrix(tst1)))
# rmsleTest2 <- rmsle(tstCost1, svmPredTest2)
# print(paste("rmsleTest2", rmsleTest2))
# 
# svmModTest3 <- ksvm(as.matrix(trn1), y = log(trnCost1+1), scaled = FALSE, #kpar = list(degree = 2),
#                     type = "eps-svr", kernel = "polydot")
# svmPredTest3 <- exp(predict(svmModTest3, as.matrix(tst1)))
# rmsleTest3 <- rmsle(tstCost1, svmPredTest3)
# print(paste("rmsleTest3", rmsleTest3))
# 
# svmModTest4 <- svm(as.matrix(trn1), y = log(trnCost1+1), scale = FALSE)
# svmPredTest4 <- exp(predict(svmModTest4, as.matrix(tst1)))
# rmsleTest4 <- rmsle(tstCost1, svmPredTest4)
# print(paste("rmsleTest4", rmsleTest4))
# 
# svmModTest5 <- svm(as.matrix(trn1[,1:14]), y = log(trnCost1+1))
# svmPredTest5 <- exp(predict(svmModTest5, as.matrix(tst1[,1:14])))
# rmsleTest5 <- rmsle(tstCost1, svmPredTest5)
# print(paste("rmsleTest5", rmsleTest5))
# 
# svmModTest6 <- svm(as.matrix(trn1[,1:14]), y = log(trnCost1+1), type = 'eps-regression', kernel = 'radial',
#                    gamma = .07, epsilon = .1, cost = 10)
# svmPredTest6 <- exp(predict(svmModTest6, as.matrix(tst1[,1:14])))
# rmsleTest6 <- rmsle(tstCost1, svmPredTest6)
# print(paste("rmsleTest6", rmsleTest6))
# 
# svmModTest7 <- svm(as.matrix(trn1[,1:14]), y = log(trnCost1+1), type = 'eps-regression', kernel = 'radial',
#                    gamma = .07, epsilon = .001, cost = 10)
# svmPredTest7 <- exp(predict(svmModTest7, as.matrix(tst1[,1:14])))
# rmsleTest7 <- rmsle(tstCost1, svmPredTest7)
# print(paste("rmsleTest7", rmsleTest7))
# 
# svmModTest8 <- svm(as.matrix(trn1[,1:14]), y = log(trnCost1+1), type = 'eps-regression', kernel = 'radial',
#                    gamma = .07, epsilon = .001, cost = 10, tolerance = .001)
# svmPredTest8 <- exp(predict(svmModTest8, as.matrix(tst1[,1:14])))
# rmsleTest8 <- rmsle(tstCost1, svmPredTest8)
# print(paste("rmsleTest8", rmsleTest8))


# require(kernlab)
# set.seed(201)
# svmFit1 <- ksvm(as.matrix(trn1), y = log(trnCost1+1), scaled = FALSE, type = "eps-svr", kernel = "rbfdot", C = 30)
# svmPred1 <- exp(predict(svmFit1, as.matrix(tst1)))-1
# rmslesvm1 <- rmsle(tstCost1, svmPred1)
# # rmsle = .6398
# svmFit2 <- ksvm(as.matrix(trn1[,1:12]), y = log(trnCost1+1), scaled = FALSE, type = "eps-svr", kernel = "rbfdot", C = 30)
# svmPred2 <- exp(predict(svmFit2, as.matrix(tst1[,1:12])))-1
# rmslesvm2 <- rmsle(tstCost1, svmPred2)
# svmFit3 <- ksvm(as.matrix(trn1[,1:12]), y = log(trnCost1+1), scaled = FALSE, type = "eps-svr", kernel = "laplacedot")
# svmPred3 <- exp(predict(svmFit3, as.matrix(tst1[,1:12])))-1
# rmslesvm3 <- rmsle(tstCost1, svmPred3)
# 
# 
# require(kknn)
# k1 = 3
# knn3Fit1 <- kknn(formula = log(trnCost1+1) ~ ., cbind(trnCost1,trn1), tst1, k = k1)
# knn3Pred1 <- exp(predict(knn3Fit1, tst1))-1
# knn3rmsle1 <- rmsle(tstCost1, knn3Pred1)
# knn3Fit2 <- kknn(formula = log(trnCost2+1) ~ ., cbind(trnCost2,trn2), tst2, k = k1)
# knn3Pred2 <- exp(predict(knn3Fit2, tst2))-1
# knn3rmsle2 <- rmsle(tstCost2, knn3Pred2)
# knn3Fit3 <- kknn(formula = log(trnCost3+1) ~ ., cbind(trnCost3,trn3), tst3, k = k1)
# knn3Pred3 <- exp(predict(knn3Fit3, tst3))-1
# knn3rmsle3 <- rmsle(tstCost3, knn3Pred3)
# knn3Fit4 <- kknn(formula = log(trnCost4+1) ~ ., cbind(trnCost4,trn4), tst4, k = k1)
# knn3Pred4 <- exp(predict(knn3Fit4, tst4))-1
# knn3rmsle4 <- rmsle(tstCost4, knn3Pred4)
# knn3Fit5 <- kknn(formula = log(trnCost5+1) ~ ., cbind(trnCost5,trn5), tst5, k = k1)
# knn3Pred5 <- exp(predict(knn3Fit5, tst5))-1
# knn3rmsle5 <- rmsle(tstCost5, knn3Pred5)
# 
# k2 = 10
# knn10Fit1 <- kknn(formula = log(trnCost1+1) ~ ., cbind(trnCost1,trn1), tst1, k = k2)
# knn10Pred1 <- exp(predict(knn10Fit1, tst1))-1
# knn10rmsle1 <- rmsle(tstCost1, knn10Pred1)
# knn10Fit2 <- kknn(formula = log(trnCost2+1) ~ ., cbind(trnCost2,trn2), tst2, k = k2)
# knn10Pred2 <- exp(predict(knn10Fit2, tst2))-1
# knn10rmsle2 <- rmsle(tstCost2, knn10Pred2)
# knn10Fit3 <- kknn(formula = log(trnCost3+1) ~ ., cbind(trnCost3,trn3), tst3, k = k2)
# knn10Pred3 <- exp(predict(knn10Fit3, tst3))-1
# knn10rmsle3 <- rmsle(tstCost3, knn10Pred3)
# knn10Fit4 <- kknn(formula = log(trnCost4+1) ~ ., cbind(trnCost4,trn4), tst4, k = k2)
# knn10Pred4 <- exp(predict(knn10Fit4, tst4))-1
# knn10rmsle4 <- rmsle(tstCost4, knn10Pred4)
# knn10Fit5 <- kknn(formula = log(trnCost5+1) ~ ., cbind(trnCost5,trn5), tst5, k = k2)
# knn10Pred5 <- exp(predict(knn10Fit5, tst5))-1
# knn10rmsle5 <- rmsle(tstCost5, knn10Pred5)
# 
# k3 = 20
# knn20Fit1 <- kknn(formula = log(trnCost1+1) ~ ., cbind(trnCost1,trn1), tst1, k = k3)
# knn20Pred1 <- exp(predict(knn20Fit1, tst1))-1
# knn20rmsle1 <- rmsle(tstCost1, knn20Pred1)
# knn20Fit2 <- kknn(formula = log(trnCost2+1) ~ ., cbind(trnCost2,trn2), tst2, k = k3)
# knn20Pred2 <- exp(predict(knn20Fit2, tst2))-1
# knn20rmsle2 <- rmsle(tstCost2, knn20Pred2)
# knn20Fit3 <- kknn(formula = log(trnCost3+1) ~ ., cbind(trnCost3,trn3), tst3, k = k3)
# knn20Pred3 <- exp(predict(knn20Fit3, tst3))-1
# knn20rmsle3 <- rmsle(tstCost3, knn20Pred3)
# knn20Fit4 <- kknn(formula = log(trnCost4+1) ~ ., cbind(trnCost4,trn4), tst4, k = k3)
# knn20Pred4 <- exp(predict(knn20Fit4, tst4))-1
# knn20rmsle4 <- rmsle(tstCost4, knn20Pred4)
# knn20Fit5 <- kknn(formula = log(trnCost5+1) ~ ., cbind(trnCost5,trn5), tst5, k = k3)
# knn20Pred5 <- exp(predict(knn20Fit5, tst5))-1
# knn20rmsle5 <- rmsle(tstCost5, knn20Pred5)
# 
# k4 = 100
# knn100Fit1 <- kknn(formula = log(trnCost1+1) ~ ., cbind(trnCost1,trn1), tst1, k = k4)
# knn100Pred1 <- exp(predict(knn100Fit1, tst1))-1
# knn100rmsle1 <- rmsle(tstCost1, knn100Pred1)
# knn100Fit2 <- kknn(formula = log(trnCost2+1) ~ ., cbind(trnCost2,trn2), tst2, k = k4)
# knn100Pred2 <- exp(predict(knn100Fit2, tst2))-1
# knn100rmsle2 <- rmsle(tstCost2, knn100Pred2)
# knn100Fit3 <- kknn(formula = log(trnCost3+1) ~ ., cbind(trnCost3,trn3), tst3, k = k4)
# knn100Pred3 <- exp(predict(knn100Fit3, tst3))-1
# knn100rmsle3 <- rmsle(tstCost3, knn100Pred3)
# knn100Fit4 <- kknn(formula = log(trnCost4+1) ~ ., cbind(trnCost4,trn4), tst4, k = k4)
# knn100Pred4 <- exp(predict(knn100Fit4, tst4))-1
# knn100rmsle4 <- rmsle(tstCost4, knn100Pred4)
# knn100Fit5 <- kknn(formula = log(trnCost5+1) ~ ., cbind(trnCost5,trn5), tst5, k = k4)
# knn100Pred5 <- exp(predict(knn100Fit5, tst5))-1
# knn100rmsle5 <- rmsle(tstCost5, knn100Pred5)
# 
# 
# out1 <- data.frame(id=as.integer(idTest), cost = exp(predOut1) - 1)
# out2 <- data.frame(id=as.integer(idTest), cost = exp(predOut2) - 1)
# write.csv(out1, file = "cat_150728_1_out1.csv", quote = F, row.names = F)
# write.csv(out2, file = "cat_150728_1_out2.csv", quote = F, row.names = F)
# 
# 
# # dtrain <- xgb.DMatrix(as.matrix(gtrain)[,-5], label = log(as.matrix(gtrain)[,5]+1))
# # if(submit==FALSE){
# #   dtest <- xgb.DMatrix(as.matrix(gtest)[,-5], label = log(as.matrix(gtest)[,5]+1))
# # } else {
# #   dtest <- xgb.DMatrix(as.matrix(gtest))
# # }
# 
# #     
# # 
# # 
# # mod1 <- xgboost(params = params, data = dtrain, nrounds = 4000)
# # preds1 <- predict(mod1,dtest)
# # 
