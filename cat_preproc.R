######################################################
### Kaggle - CAT Tube Quoting ########################
######################################################

### Test or Submission Setting #######################
submit = FALSE ### Setting to FALSE activates testing

### Set Working Directory ############################
setwd("C:/Projects/catCleanup/")

### Set up Parallel Solver ###########################
require(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

### Function for Calculating RMSLE ###################
# if (submit == FALSE){
  rmsle <- function(reference, prediction){
    n <- length(prediction)
    rmsle <- ((1/n) * sum((log(prediction + 1) - log(reference + 1))^2))^.5
    return(rmsle)
  }

### Constants & Parameters ###########################
pi = 3.1416

datadir <- "competition_data"
BOM <- read.csv(paste0(datadir,"/bill_of_materials.csv"), as.is = T)
adapt <- read.csv(paste0(datadir,"/comp_adaptor.csv"), as.is = T)
boss <- read.csv(paste0(datadir,"/comp_boss.csv"), as.is = T)
elbow <- read.csv(paste0(datadir,"/comp_elbow.csv"), as.is = T)
float <- read.csv(paste0(datadir,"/comp_float.csv"), as.is = T)
hfl <- read.csv(paste0(datadir,"/comp_hfl.csv"), as.is = T)
nut <- read.csv(paste0(datadir,"/comp_nut.csv"), as.is = T)
other <- read.csv(paste0(datadir,"/comp_other.csv"), as.is = T)
sleeve <- read.csv(paste0(datadir,"/comp_sleeve.csv"), as.is = T)
straight <- read.csv(paste0(datadir,"/comp_straight.csv"), as.is = T)
tee <- read.csv(paste0(datadir,"/comp_tee.csv"), as.is = T)
thread <- read.csv(paste0(datadir,"/comp_threaded.csv"), as.is = T)
comps <- read.csv(paste0(datadir,"/components.csv"), as.is = T, quote = "")
spec <- read.csv(paste0(datadir,"/specs.csv"), as.is = T)
test <- read.csv(paste0(datadir,"/test_set.csv"), as.is = T)
train <- read.csv(paste0(datadir,"/train_set.csv"), as.is = T)
tube <- read.csv(paste0(datadir,"/tube.csv"), as.is = T)
tubeEnd <- read.csv(paste0(datadir,"/tube_end_form.csv"), as.is = T)
typeComp <- read.csv(paste0(datadir,"/type_component.csv"), as.is = T)
typeCon <- read.csv(paste0(datadir,"/type_connection.csv"), as.is = T)
typeEnd <- read.csv(paste0(datadir,"/type_end_form.csv"),as.is = T)

cost <- train$cost; train$cost <- NULL
idTrain <- seq(length(cost))
idTest <- test$id; test$id <- NULL
colnames(train)

### Correct Some Tube Lengths ######################
tube$length[tube$tube_assembly_id=="TA-00152"] <- 19
tube$length[tube$tube_assembly_id=="TA-00154"] <- 75
tube$length[tube$tube_assembly_id=="TA-00156"] <- 24
tube$length[tube$tube_assembly_id=="TA-01098"] <- 10
tube$length[tube$tube_assembly_id=="TA-01631"] <- 48
tube$length[tube$tube_assembly_id=="TA-03520"] <- 46
tube$length[tube$tube_assembly_id=="TA-04114"] <- 135
tube$length[tube$tube_assembly_id=="TA-17390"] <- 40
tube$length[tube$tube_assembly_id=="TA-18227"] <- 74
tube$length[tube$tube_assembly_id=="TA-18229"] <- 51

### Factor Level Setting #############################
suppliers <- sort(unique(c(as.character(train$supplier),as.character(test$supplier))))
bracketLevs <- sort(unique(c(train$bracket_pricing,test$bracket_pricing)))
tubeMatls <- c(sort(unique(tube$material_id)),"unknown")
tubeEnds <- c(sort(unique(c(tube$end_a,tube$end_x))))

### Adjustments & Additions ##########################
comps$weight <- adapt$weight[match(comps$component_id, adapt$component_id)]
comps$weight[is.na(comps$weight)] <- boss$weight[match(comps$component_id[is.na(comps$weight)], boss$component_id)]
comps$weight[is.na(comps$weight)] <- elbow$weight[match(comps$component_id[is.na(comps$weight)], elbow$component_id)]
comps$weight[is.na(comps$weight)] <- float$weight[match(comps$component_id[is.na(comps$weight)], float$component_id)]
comps$weight[is.na(comps$weight)] <- hfl$weight[match(comps$component_id[is.na(comps$weight)], hfl$component_id)]
comps$weight[is.na(comps$weight)] <- nut$weight[match(comps$component_id[is.na(comps$weight)], nut$component_id)]
comps$weight[is.na(comps$weight)] <- other$weight[match(comps$component_id[is.na(comps$weight)], other$component_id)]
comps$weight[is.na(comps$weight)] <- sleeve$weight[match(comps$component_id[is.na(comps$weight)], sleeve$component_id)]
comps$weight[is.na(comps$weight)] <- straight$weight[match(comps$component_id[is.na(comps$weight)], straight$component_id)]
comps$weight[is.na(comps$weight)] <- tee$weight[match(comps$component_id[is.na(comps$weight)], tee$component_id)]
comps$weight[is.na(comps$weight)] <- thread$weight[match(comps$component_id[is.na(comps$weight)], thread$component_id)]
comps$weight[is.na(comps$weight)] <- 0
comps$name[(is.na(comps$name))] <- "OTHER"
BOM$quantity_1[is.na(BOM$quantity_1)] <- 0
BOM$quantity_2[is.na(BOM$quantity_2)] <- 0
BOM$quantity_3[is.na(BOM$quantity_3)] <- 0
BOM$quantity_4[is.na(BOM$quantity_4)] <- 0
BOM$quantity_5[is.na(BOM$quantity_5)] <- 0
BOM$quantity_6[is.na(BOM$quantity_6)] <- 0
BOM$quantity_7[is.na(BOM$quantity_7)] <- 0
BOM$quantity_8[is.na(BOM$quantity_8)] <- 0
BOM[is.na(BOM)] <- "none"
BOM$weight <- 0
for (i in 1:nrow(BOM)){
  asmID <- BOM$tube_assembly_id[i]
  for (j in 1:8){
    if (BOM[i,2*j]!="none"){
      compID <- BOM[i,2*j]
      BOM$weight[i] <- BOM$weight[i] + comps$weight[match(compID,comps$component_id)]*BOM[i,2*j+1]
    }
  }
}
BOM$qty <- BOM$quantity_1+BOM$quantity_2+BOM$quantity_3+BOM$quantity_4+BOM$quantity_5+BOM$quantity_6+BOM$quantity_7+BOM$quantity_8
tube$vol <- 2*pi*tube$diameter*tube$wall*tube$length
tube$material_id[is.na(tube$material_id)] <- "none"
suppliers <- data.frame(supplier = suppliers, qty = 0, stringsAsFactors = F)
count = nrow(suppliers)*nrow(train)
for (i in 1:nrow(suppliers)){
  for (j in 1:nrow(train)){
    if(train$supplier[j]==suppliers$supplier[i]){
      suppliers$qty[i] <- suppliers$qty[i] + train$annual_usage[j]
    }
  }
  if (i %% 17 == 0){
    print(paste0("Populating Supplier Qty (Train): ",as.integer(1000*i/nrow(suppliers))/10,"%"))
  }
}
for (i in 1:nrow(suppliers)){
  for (k in 1:nrow(test)){
    if(test$supplier[k]==suppliers$supplier[i]){
      suppliers$qty[i] <- suppliers$qty[i] + test$annual_usage[k]
    }
  }
  if (i %% 17 == 0){
    print(paste0("Populating Supplier Qty (Test):",as.integer(1000*i/nrow(suppliers))/10,"%"))
  }
}

# Combine all specs and establish spec frequencies ###
allSpec <- sort(table(stack(spec[,2:11])[,1]), decreasing = T)
sigSpecs <- names(allSpec)[allSpec>300]
sigSpecs <- gsub("-","_",sigSpecs)
sigSpecsDash <- gsub("_","-",sigSpecs)

tubeEnd$end_form_id <- gsub("-","_",tubeEnd$end_form_id)
tubeEnd <- rbind(tubeEnd,c("NONE","No"))

### Feature Matrix Adjustment
featureAdjust <- function(X){
  X$quote_date <- as.numeric(as.Date(X$quote_date))
  X$BOMqty <- BOM$qty[match(X$tube_assembly_id, BOM$tube_assembly_id)]
  X$BOMweight <- BOM$weight[match(X$tube_assembly_id, BOM$tube_assembly_id)]
  X$supplierQty <- suppliers$qty[match(X$supplier, suppliers$supplier)]
  X$supplier <- factor(X$supplier, levels = suppliers$supplier)
  X$bracket_pricing <- factor(X$bracket_pricing, levels = bracketLevs)
  X$tubeMatl <- tube$material_id[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$tubeMatl <- factor(X$tubeMatl, levels = tubeMatls)
  X$tubeMatl[is.na(X$tubeMatl)] <- "unknown"
  X$diameter <- tube$diameter[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$wall <- tube$wall[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$length <- tube$length[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$numBends <- tube$num_bends[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$vol <- tube$vol[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$bendRadius <- tube$bend_radius[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$endA1X <- as.factor(tube$end_a_1x[match(X$tube_assembly_id,tube$tube_assembly_id)])
  X$endA2X <- as.factor(tube$end_a_2x[match(X$tube_assembly_id,tube$tube_assembly_id)])
  X$endX1X <- as.factor(tube$end_x_1x[match(X$tube_assembly_id,tube$tube_assembly_id)])
  X$endX2X <- as.factor(tube$end_x_2x[match(X$tube_assembly_id,tube$tube_assembly_id)])
  X$endA <- factor(tube$end_a[match(X$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
  X$endX <- factor(tube$end_x[match(X$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
  X$numBoss <- tube$num_boss[match(X$tube_assembly_id,tube$tube_assembly_id)]
  X$numBracket <- tube$num_bracket[match(X$tube_assembly_id,tube$tube_assembly_id)]
  
  for (i in 1:length(sigSpecs)){
    X[sigSpecs[i]] <- 0
  }
  
  for (j in 1:length(sigSpecs)){
    for (i in 1:nrow(X)){ 
      if (sigSpecsDash[j] %in% spec[spec$tube_assembly_id==X$tube_assembly_id[i],]){
        X[i,sigSpecs[j]] = X[i,sigSpecs[j]] + 1
      }
      if (i %% 30000 == 0){
        print(paste0(as.integer(1000*((j-1)*nrow(X)+i)/(length(sigSpecs)*nrow(X)))/10,'% Complete'))
      }
    }
  }
  
  tube_Matls <- gsub("-","_",tubeMatls)
  for (i in 1:length(tube_Matls)){
    X[tube_Matls[i]] <- 0
  }
  for (j in 1:length(tubeMatls)){
    for (i in 1:nrow(X)){ 
      if (X$tubeMatl[i] == tubeMatls[j]){
        X[i,tube_Matls[j]] = 1
      } else {X[i,tube_Matls[j]] = 0}
      if (i %% 30000 == 0){
        print(paste0(as.integer(1000*((j-1)*nrow(X)+i)/(length(tubeMatls)*nrow(X)))/10,'% Complete'))
      }
    }
  }
  
  X$endA <- gsub("-","_",X$endA)
  X$endX <- gsub("-","_",X$endX)
  
  for (i in 1:length(tubeEnd$end_form_id)){
    X[tubeEnd$end_form_id[i]] <- 0
  }
  for (i in 1:nrow(X)){
    for (j in 1:length(tubeEnd$end_form_id)){
      if(as.character(X$endA[i])==tubeEnd$end_form_id[j]){
        X[i,tubeEnd$end_form_id[j]] = X[i,tubeEnd$end_form_id[j]] + 1
      }
    }
  }
  for (i in 1:nrow(X)){
    for (j in 1:length(tubeEnd$end_form_id)){
      if(as.character(X$endX[i])==tubeEnd$end_form_id[j]){
        X[i,tubeEnd$end_form_id[j]] = X[i,tubeEnd$end_form_id[j]] + 1
      }
    }
  }
  return(X)
}

train2 <- featureAdjust(train)
test2 <- featureAdjust(test)

write.csv(train2, file = 'cat_train2.csv')
write.csv(test2, file = 'cat_test2.csv')


##########################
## XGBOOST ###############
##########################

# gtrain <- train2
# gtest <- test2
# 
# gtrain$tube_assembly_id = NULL; gtrain$supplier = NULL
# gtrain$bracket_pricing = NULL; gtrain$tubeMatl = NULL
# gtrain$endA = NULL; gtrain$endX = NULL
# gtrain$endA1X = NULL; gtrain$endA2X = NULL
# gtrain$endX1X = NULL; gtrain$endX2X = NULL
# 
# # if (submit == T){
#   gtest$tube_assembly_id = NULL; gtest$supplier = NULL
#   gtest$bracket_pricing = NULL; gtest$tubeMatl = NULL
#   gtest$endA = NULL; gtest$endX = NULL
#   gtest$endA1X = NULL; gtest$endA2X = NULL
#   gtest$endX1X = NULL; gtest$endX2X = NULL
# # }
# 
# ### Create CV Folds ################################
# # if (submit ==FALSE){
#   require(caret)
#   set.seed(1301)
#   cvFolds <- createFolds(cost, k = 5, list = T, returnTrain = F)
#   trn1 <- gtrain[-cvFolds[[1]],]; tst1 <- gtrain[cvFolds[[1]],]
#   trnCost1 <- cost[-cvFolds[[1]]]; tstCost1 <- cost[cvFolds[[1]]]
#   trn2 <- gtrain[-cvFolds[[2]],]; tst2 <- gtrain[cvFolds[[2]],]
#   trnCost2 <- cost[-cvFolds[[2]]]; tstCost2 <- cost[cvFolds[[2]]]
#   trn3 <- gtrain[-cvFolds[[3]],]; tst3 <- gtrain[cvFolds[[3]],]
#   trnCost3 <- cost[-cvFolds[[3]]]; tstCost3 <- cost[cvFolds[[3]]]
#   trn4 <- gtrain[-cvFolds[[4]],]; tst4 <- gtrain[cvFolds[[4]],]
#   trnCost4 <- cost[-cvFolds[[4]]]; tstCost4 <- cost[cvFolds[[4]]]
#   trn5 <- gtrain[-cvFolds[[5]],]; tst5 <- gtrain[cvFolds[[5]],]
#   trnCost5 <- cost[-cvFolds[[5]]]; tstCost5 <- cost[cvFolds[[5]]]
#   trn <- gtrain; trnCost <- cost; tst <- gtest
# # }
# 
# require(xgboost); require(Matrix); require(data.table)
# 
# dtrn <- xgb.DMatrix(as.matrix(trn), label = log(as.matrix(trnCost)+1))
# dtrn1 <- xgb.DMatrix(as.matrix(trn1), label = log(as.matrix(trnCost1)+1))
# dtrn2 <- xgb.DMatrix(as.matrix(trn2), label = log(as.matrix(trnCost2)+1))
# dtrn3 <- xgb.DMatrix(as.matrix(trn3), label = log(as.matrix(trnCost3)+1))
# dtrn4 <- xgb.DMatrix(as.matrix(trn4), label = log(as.matrix(trnCost4)+1))
# dtrn5 <- xgb.DMatrix(as.matrix(trn5), label = log(as.matrix(trnCost5)+1))
# 
# dtst <- xgb.DMatrix(as.matrix(tst))
# dtst1 <- xgb.DMatrix(as.matrix(tst1))
# dtst2 <- xgb.DMatrix(as.matrix(tst2))
# dtst3 <- xgb.DMatrix(as.matrix(tst3))
# dtst4 <- xgb.DMatrix(as.matrix(tst4))
# dtst5 <- xgb.DMatrix(as.matrix(tst5))
# 
# params <- list(booster = "gbtree",
#                eta = .1, # was .05
#                min_child_weight = 1, # was 1
#                subsample = .8, # was .8
#                colsample_bytree = .8, # was .8
#                max_depth = 6, # was 7
#                objective = "reg:linear", # was reg:linear
#                base_score = .5) # was base_score
# num_rounds = 3000
# set.seed(901)
# mod1 <- xgboost(params = params, data = dtrn1, nrounds = num_rounds, verbose = 1, print.every.n = 50)
# preds1 <- predict(mod1,dtst1)
# rmsle1 <- rmsle(tstCost1, exp(preds1)-1)
# set.seed(902)
# mod2 <- xgboost(params = params, data = dtrn2, nrounds = num_rounds, verbose = 1, print.every.n = 50)
# preds2 <- predict(mod2,dtst2)
# rmsle2 <- rmsle(tstCost2, exp(preds2)-1)
# set.seed(903)
# mod3 <- xgboost(params = params, data = dtrn3, nrounds = num_rounds, verbose = 1, print.every.n = 50)
# preds3 <- predict(mod3,dtst3)
# rmsle3 <- rmsle(tstCost3, exp(preds3)-1)
# set.seed(904)
# mod4 <- xgboost(params = params, data = dtrn4, nrounds = num_rounds, verbose = 1, print.every.n = 50)
# preds4 <- predict(mod4,dtst4)
# rmsle4 <- rmsle(tstCost4, exp(preds4)-1)
# set.seed(905)
# mod5 <- xgboost(params = params, data = dtrn5, nrounds = num_rounds, verbose = 1, print.every.n = 50)
# preds5 <- predict(mod5,dtst5)
# rmsle5 <- rmsle(tstCost5, exp(preds5)-1)
# print(c(rmsle1, rmsle2, rmsle3, rmsle4, rmsle5))
# print(mean(c(rmsle1, rmsle2, rmsle3, rmsle4, rmsle5)))
# 
# set.seed(1311)
# mod6 <- xgboost(params = params, data = dtrn, nrounds = num_rounds)
# 
# P1 <- predict(mod1, dtst)
# P2 <- predict(mod2, dtst)
# P3 <- predict(mod3, dtst)
# P4 <- predict(mod4, dtst)
# P5 <- predict(mod5, dtst)
# P6 <- predict(mod6, dtst)
# 
# predOut1 <- (P1 + P2 + P3 + P4 + P5 + P6) / 6
# predOut2 <- (P6*(P1^.2)*(P2^.2)*(P3^.2)*(P4^.2)*(P5^.2))^.5
# plot(abs(predOut1-predOut2))
# plot(predOut1,predOut2)
# 
# out1 <- data.frame(id=as.integer(idTest), cost = exp(predOut1) - 1)
# out2 <- data.frame(id=as.integer(idTest), cost = exp(predOut2) - 1)
# write.csv(out1, file = "cat_150728_1_out1.csv", quote = F, row.names = F)
# write.csv(out2, file = "cat_150728_1_out2.csv", quote = F, row.names = F)