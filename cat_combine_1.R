######################################################
### Kaggle - CAT Tube Quoting ########################
######################################################

### Test or Submission Setting #######################
submit = TRUE ### Setting to FALSE activates testing

### Set Working Directory ############################
setwd("C:/Projects/catCleanup/")

### Set up Parallel Solver ###########################
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

### Function for Calculating RMSLE ###################
if (submit == FALSE){
  rmsle <- function(reference, prediction){
    n <- length(prediction)
    rmsle <- ((1/n) * sum((log(prediction + 1) - log(reference + 1))^2))^.5
    return(rmsle)
  }
}

### Constants & Parameters ###########################
pi = 3.1416

### Load Competition Data ############################
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

### Create trn/chk Data Partitions #################
if (submit==FALSE){
  require(caret)
  set.seed(1331)
  trnIndex <- createDataPartition(train$cost, p = .8, list = F)
  trn <- train[trnIndex,]; chk <- train[-trnIndex,]
}

### Set Model Data Set #############################
if (submit==FALSE){
  train <- trn  ## Deactivate for submission
  test <- chk   ## Deactivate for submission
}
  
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
comp_vars <- c('adapt','boss','elbow','float','hfl','nut','other','sleeve','tee','thread')
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
for (i in 1:nrow(suppliers)){
  for (j in 1:nrow(train)){
    if(train$supplier[j]==suppliers$supplier[i]){
      suppliers$qty[i] <- suppliers$qty[i] + train$annual_usage[j]
    }
  }
}

sigSuppliers <- names(sort(table(c(train$supplier,test$supplier)), decreasing = T)[1:30])

### Training Matrix Adjustment
train$quote_date <- as.numeric(as.Date(train$quote_date))
train$BOMqty <- BOM$qty[match(train$tube_assembly_id, BOM$tube_assembly_id)]
train$BOMweight <- BOM$weight[match(train$tube_assembly_id, BOM$tube_assembly_id)]
train$supplierQty <- suppliers$qty[match(train$supplier, suppliers$supplier)]
train$supplier <- factor(train$supplier, levels = suppliers$supplier)
train$bracket_pricing <- factor(train$bracket_pricing, levels = bracketLevs)
train$tubeMatl <- tube$material_id[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$tubeMatl <- factor(train$tubeMatl, levels = tubeMatls)
train$tubeMatl[is.na(train$tubeMatl)] <- "unknown"
train$diameter <- tube$diameter[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$wall <- tube$wall[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$length <- tube$length[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$numBends <- tube$num_bends[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$vol <- tube$vol[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$bendRadius <- tube$bend_radius[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$endA1X <- as.factor(tube$end_a_1x[match(train$tube_assembly_id,tube$tube_assembly_id)])
train$endA2X <- as.factor(tube$end_a_2x[match(train$tube_assembly_id,tube$tube_assembly_id)])
train$endX1X <- as.factor(tube$end_x_1x[match(train$tube_assembly_id,tube$tube_assembly_id)])
train$endX2X <- as.factor(tube$end_x_2x[match(train$tube_assembly_id,tube$tube_assembly_id)])
train$endA <- factor(tube$end_a[match(train$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
train$endX <- factor(tube$end_x[match(train$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
train$numBoss <- tube$num_boss[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$numBracket <- tube$num_bracket[match(train$tube_assembly_id,tube$tube_assembly_id)]
train$supplier <- factor(train$supplier, levels = c(sigSuppliers,"rare"))
train$supplier[is.na(train$supplier)] <- "rare"

# Combine all specs and establish spec frequencies ###
allSpec <- sort(table(stack(spec[,2:11])[,1]), decreasing = T)
sigSpecs <- names(allSpec)[allSpec>300]
sigSpecs <- gsub("-","_",sigSpecs)
sigSpecsDash <- gsub("_","-",sigSpecs)

#### Test Section
specDF <- data.frame(tube_assembly_id = train$tube_assembly_id, stringsAsFactors = F)
for (i in 1:length(sigSpecs)){specDF[sigSpecs[i]] <- 0} # Create columns
maxcount = nrow(train)
for (i in 1:nrow(train)){
  if ((i==1)||(specDF$tube_assembly_id[i] != specDF$tube_assembly_id[i-1])){
    specRow <- which(spec$tube_assembly_id==specDF$tube_assembly_id[i])
    for (j in 1:length(sigSpecs)){
      if (sigSpecsDash[j] %in% spec[specRow,]){
        specDF[i,sigSpecs[j]] = specDF[i,sigSpecs[j]] + 1
      }
    }
  } else {specDF[i,] = specDF[i-1,]}
  if (i %% 1000 == 0){
    print(paste0(as.integer(100*i/maxcount),'% Complete'))
  }
}

for (i in 1:length(sigSpecs)){
  train[sigSpecs[i]] <- specDF[sigSpecs[i]]
}

tube_Matls <- gsub("-","_",tubeMatls)
for (i in 1:length(tube_Matls)){
  train[tube_Matls[i]] <- 0
}
for (j in 1:length(tubeMatls)){
  for (i in 1:nrow(train)){ 
    if (train$tubeMatl[i] == tubeMatls[j]){
      train[i,tube_Matls[j]] = 1
    } else {train[i,tube_Matls[j]] = 0}
    if (i %% 10000 == 0){
      print(paste0(as.integer(1000*((j-1)*nrow(train)+i)/(length(tubeMatls)*nrow(train)))/10,'% Complete'))
    }
  }
}

### Testing Matrix Adjustment ###########################

test$quote_date <- as.numeric(as.Date(test$quote_date))
test$BOMqty <- BOM$qty[match(test$tube_assembly_id, BOM$tube_assembly_id)]
test$BOMweight <- BOM$weight[match(test$tube_assembly_id, BOM$tube_assembly_id)]
test$supplierQty <- suppliers$qty[match(test$supplier, suppliers$supplier)]
test$supplier <- factor(test$supplier, levels = suppliers$supplier)
test$bracket_pricing <- factor(test$bracket_pricing, levels = bracketLevs)
test$tubeMatl <- tube$material_id[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$tubeMatl <- factor(test$tubeMatl, levels = tubeMatls)
test$tubeMatl[is.na(test$tubeMatl)] <- "unknown"
test$diameter <- tube$diameter[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$wall <- tube$wall[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$length <- tube$length[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$numBends <- tube$num_bends[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$vol <- tube$vol[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$bendRadius <- tube$bend_radius[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$endA1X <- as.factor(tube$end_a_1x[match(test$tube_assembly_id,tube$tube_assembly_id)])
test$endA2X <- as.factor(tube$end_a_2x[match(test$tube_assembly_id,tube$tube_assembly_id)])
test$endX1X <- as.factor(tube$end_x_1x[match(test$tube_assembly_id,tube$tube_assembly_id)])
test$endX2X <- as.factor(tube$end_x_2x[match(test$tube_assembly_id,tube$tube_assembly_id)])
test$endA <- factor(tube$end_a[match(test$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
test$endX <- factor(tube$end_x[match(test$tube_assembly_id,tube$tube_assembly_id)],levels=tubeEnds)
test$numBoss <- tube$num_boss[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$numBracket <- tube$num_bracket[match(test$tube_assembly_id,tube$tube_assembly_id)]
test$supplier <- factor(test$supplier, levels = c(sigSuppliers,"rare"))
test$supplier[is.na(test$supplier)] <- "rare"

specDF <- data.frame(tube_assembly_id = test$tube_assembly_id, stringsAsFactors = F)
for (i in 1:length(sigSpecs)){specDF[sigSpecs[i]] <- 0} # Create columns
maxcount = nrow(test)
for (i in 1:nrow(test)){
  if ((i==1)||(specDF$tube_assembly_id[i] != specDF$tube_assembly_id[i-1])){
    specRow <- which(spec$tube_assembly_id==specDF$tube_assembly_id[i])
    for (j in 1:length(sigSpecs)){
      if (sigSpecsDash[j] %in% spec[specRow,]){
        specDF[i,sigSpecs[j]] = specDF[i,sigSpecs[j]] + 1
      }
    }
  } else {specDF[i,] = specDF[i-1,]}
  if (i %% 1000 == 0){
    print(paste0(as.integer(100*i/maxcount),'% Complete'))
  }
}

for (i in 1:length(sigSpecs)){
  test[sigSpecs[i]] <- specDF[sigSpecs[i]]
}

rm(i, sigSpecs, specDF, maxcount)

for (i in 1:length(tube_Matls)){
  test[tube_Matls[i]] <- 0
}
for (j in 1:length(tubeMatls)){
  for (i in 1:nrow(test)){ 
    if (test$tubeMatl[i] == tubeMatls[j]){
      test[i,tube_Matls[j]] = 1
    } else {test[i,tube_Matls[j]] = 0}
    if (i %% 10000 == 0){
      print(paste0(as.integer(1000*((j-1)*nrow(test)+i)/(length(tubeMatls)*nrow(test)))/10,'% Complete'))
    }
  }
}

##### Test Section
train$vol <- log(train$vol + 1); test$vol <- log(test$vol + 1)
train$length <- log(train$length + 1); test$length <- log(test$length + 1)
train$BOMweight <- log(log(train$BOMweight+1)+1)
test$BOMweight <- log(log(test$BOMweight+1)+1)
train$bendRadius <- log(train$bendRadius+1); test$bendRadius <- log(test$bendRadius+1)
train$annual_usage <- log(train$annual_usage+1); test$annual_usage <- log(test$annual_usage+1)
train$min_order_quantity <- log(train$min_order_quantity+1)
test$min_order_quantity <- log(test$min_order_quantity+1)


##### End Test Section

gtrain <- train; gtest <- test

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

require(xgboost); require(Matrix); require(data.table)

dtrain <- xgb.DMatrix(as.matrix(gtrain)[,-5], label = log(as.matrix(gtrain)[,5]+1))
if(submit==FALSE){
  dtest <- xgb.DMatrix(as.matrix(gtest)[,-5], label = log(as.matrix(gtest)[,5]+1))
} else {
  dtest <- xgb.DMatrix(as.matrix(gtest))
}

params <- list(booster = "gbtree",
            silent = 1, # was 1
            eta = .03, # was .05
            min_child_weight = 1, # was 1
            subsample = .8, # was .8
            colsample_bytree = .8, # was .8
            max_depth = 8, # was 7
            objective = "reg:linear", # was reg:linear
            base_score = .5) # was base_score

mod1 <- xgboost(params = params, data = dtrain, nrounds = 4000)
preds1 <- predict(mod1,dtrain)

mod2 <- xgboost(params = params, data = dtrain, nrounds = 4500)
preds2 <- predict(mod2,dtrain)

mod3 <- xgboost(params = params, data = dtrain, nrounds = 5000)
preds3 <- predict(mod3,dtrain)

preds <- exp((preds1+preds2+preds3)/3)-1

require(randomForest)
mod4 <- randomForest(cbind(train[2:7],train[9:60]),log(train[,8]+1), ntree = 50)
preds4 <- exp(predict(mod4, train)) - 1

require(kernlab)
mod6 <- ksvm(log(cost+1) ~ .-tube_assembly_id, data = train,
             type = "eps-svr", kernel = "rbfdot", C = 30)
preds6 <- exp(predict(mod6, train))-1

htrain <- train
htrain$preds <- log(preds+1)
htrain$preds4 <- log(preds4+1)
htrain$preds6 <- log(preds6+1)

htest <- test
htest$preds <- predict(mod1,dtest)
htest$preds4 <- predict(mod4, test)
htest$preds6 <- predict(mod6, test)

stack1 <- randomForest(log(cost+1) ~ .-tube_assembly_id, data = htrain, ntree = 50)
predsS1 <- predict(stack1, htest)

### Create Submission File #########################
out <- data.frame(id=as.integer(rownames(test)),cost = exp(predsS1)-1)
cat_xgb_1 <- read.csv("cat_xgb_1.csv", as.is = T)
cat_xgb_2 <- read.csv("cat_xgb_2.csv", as.is = T)
cat_xgb_3 <- read.csv("cat_xgb_3.csv", as.is = T)
out2 <- data.frame(id=as.integer(rownames(test)),
                   cost = ((out$cost+cat_150718_2[,2]+
                                cat_150717_3[,2]+cat_150717_4[,2])/4))
write.csv(out2, file = "cat_combine_1.csv", quote = F, row.names = F)
