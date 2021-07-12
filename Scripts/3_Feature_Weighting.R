tool_exec <- function(in_params, out_params)
{
  #####################################################################################################  
  ### Check/Load Required Packages  
  #####################################################################################################   
  options(repos="https://CRAN.R-project.org")
  set.seed(24)
  round(memory.limit()/2^20, 2)
  library(arcgisbinding)
  arc.check_product()
  arc.progress_label("Loading Libraries...")
  arc.progress_pos(0)

if (!requireNamespace("rgdal", quietly = TRUE))
  install.packages("rgdal")
if (!requireNamespace("raster", quietly = TRUE))
  install.packages("raster")
if (!requireNamespace("sp", quietly = TRUE))
  install.packages("sp")
if (!requireNamespace("FSelector", quietly = TRUE))
  install.packages("FSelector")
if (!requireNamespace("xlsx", quietly = TRUE))
  install.packages("xlsx")
if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr")
if (!requireNamespace("svDialogs", quietly = TRUE))
  install.packages("svDialogs")
if (!requireNamespace("rgeos", quietly = TRUE))
  install.packages("rgeos")
if (!requireNamespace("mlbench", quietly = TRUE))
  install.packages("mlbench")
if (!requireNamespace("caret", quietly = TRUE))
  install.packages("caret")
if (!requireNamespace("Boruta", quietly = TRUE))
    install.packages("Boruta")
if (!requireNamespace("doParallel", quietly = TRUE))
    install.packages("doParallel")
if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
if (!requireNamespace("minerva", quietly = TRUE))
    install.packages("minerva")
if (!requireNamespace("data.table", quietly = TRUE))
    install.packages("data.table")

require(data.table)  
require(rgdal)
require(raster)
require(sp)
require(FSelector)
require(xlsx)
require(dplyr)
require(svDialogs)
require(rgeos)
require(mlbench)
require(caret)
require(Boruta)
require(doParallel)
require(svDialogs)
require(minerva)

### Functions
  
  #function_1
  FeatureData <- function(features,train){
    pol <- rasterToPolygons(train,dissolve=TRUE)
    train1 <- rasterize(pol,field = names(train),features, updateValue= "NA")
  
    predictors<-stack(features,train1)
    names(predictors)[length(names(predictors))]<-"train"
    names(predictors)
    
    value_table=getValues(predictors)
    value_table=na.omit(value_table)
    value_table=as.data.frame(value_table)
    value_table$train <- rounded_value(value_table$train)
    return(value_table)
    }  
 
  #function_2
  rounded_value <- function(value) {
    value <- round(value,digits = 0)
    return (value)
    }

  #function_3
  esitSayidaAlma <- function(value_table){
    maxverisayisi <- min(table(value_table$train)) * 2
    trainsayisi <- maxverisayisi
    trainid <- createSets(value_table,value_table$train,trainsayisi)
    traindata <- value_table[trainid,]
    return(traindata)
    }
 
  #function_4
 createSets <- function(x, y, p){
   nr <- NROW(x)
   size <- (p) %/% length(unique(y))
   idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size))
   unlist(idx)
    } 

  rgdal::set_thin_PROJ6_warnings(TRUE)
  rfiles1 <- in_params[[1]]
  algoritm <- in_params[[2]]
  trainPath <- in_params[[3]]

  kayitPath <-out_params[[1]]
  writeXls<-out_params[[2]]


  if(algoritm == "xgbLinear" | algoritm == "xgbTree" ){
  ########## Uyarý Mesajý ############
  warningMessage <- dlgMessage("Attention All CPU Cores will be used for this operation .Do you want to contunieu to process?", type = "yesno", gui = .GUI)$res
  if(warningMessage == "no"){
    return(out_params)
  }
  
  no_cores <- detectCores() - 1
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  }


train <- raster(trainPath)

allrasters <- lapply(rfiles1, stack)

allWtrain <-append(allrasters,train)

nlist <- length(allWtrain[-1])

valueDFlist <- lapply(1:nlist, function(x) NULL)

arc.progress_label("Training Set is creating...")
arc.progress_pos(20)

for(i in 1:nlist){
  
  valueDFlist[[i]] <- FeatureData(allWtrain[[i]] , last(allWtrain))
}

trainSampleList <- lapply(1:nlist, function(x) NULL)

for (i in 1:nlist) {
  
  trainSampleList[[i]] <- esitSayidaAlma(valueDFlist[[i]])
  }


#lapply(trainSampleList, function(x) replace(x, is.na(x%%1==0), 0))

rankCorrWeigthList <- lapply(1:nlist, function(x) NULL)
WeigthList <- lapply(1:nlist, function(x) NULL)
carnList <- lapply(1:nlist, function(x) NULL)
FeatureImportave <- lapply(1:nlist, function(x) NULL)
BarotaWeigthList <- lapply(1:nlist, function(x) NULL)
BarotaWeigthList2 <- lapply(1:nlist, function(x) NULL)
DFlist <- lapply(1:nlist, function(x) NULL)
for(i in 1:nlist){
  DFlist[[i]] <- trainSampleList[[i]][ , -((ncol(trainSampleList[[i]]) ):ncol(trainSampleList[[i]]))]
}

control <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = T)

arc.progress_label("Calculating Weights...")
arc.progress_pos(40)



if(algoritm == "Chi_Square"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- chi.squared(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Information_Gain"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- information.gain(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Boruta"){
  for (i in 1:nlist) { 
    FeatureImportave[[i]] <- Boruta(train~., data = trainSampleList[[i]], doTrace = 0, getImp = getImpRfZ,pValue = 0.05)
    BarotaWeigthList[[i]] <- as.data.frame(FeatureImportave[[i]]$ImpHistory[nrow(FeatureImportave[[i]]$ImpHistory),])
    BarotaWeigthList2[[i]] <- as.data.frame(BarotaWeigthList[[i]][(row.names(BarotaWeigthList[[i]]) %in% names(allWtrain[[i]])),])
    row.names(BarotaWeigthList2[[i]]) <- names(allWtrain[[i]])
    rankCorrWeigthList<-BarotaWeigthList2
    print((i/nlist)*100)
    
  }
}else if(algoritm == "Gain_Ratio"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- gain.ratio(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Symmetrical_Uncertainty"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- symmetrical.uncertainty(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Linear_Correlation"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- linear.correlation(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Rank_Correlation"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- rank.correlation(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "Random_Forest"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- random.forest.importance(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "OneR"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- oneR(train ~., trainSampleList[[i]])
    print((i/nlist)*100)
  }
}else if(algoritm == "null"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="null", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "nnls"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="nnls", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "lm"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="lm", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "glmStepAIC"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="glmStepAIC", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "xgbTree"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="xgbTree", preProcess="scale", trControl=control,allowParallel=T)
    print((i/nlist)*100)
    }
  
}else if(algoritm == "xgbLinear"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="xgbLinear", preProcess="scale", trControl=control,allowParallel=T)
    print((i/nlist)*100)
    }
}else if(algoritm == "lmStepAIC"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="lmStepAIC", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "blassoAveraged"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="blassoAveraged", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }
}else if(algoritm == "MIC"){
  for (i in 1:nlist) {
    rankCorrWeigthList[[i]] <- (mine(x=DFlist[[i]],y=trainSampleList[[i]]$train, alpha=0.7))$MIC
    print((i/nlist)*100)
  }
}else if(algoritm == "bridge"){
  for (i in 1:nlist) {
    WeigthList[[i]] <- train(train ~., data=trainSampleList[[i]], method="bridge", preProcess="scale", trControl=control,allowParallel=F)
    print((i/nlist)*100)
  }

}else cat("Please choose an algorithm...")


if(algoritm == "xgbLinear" | algoritm == "xgbTree" ){
  ########## Uyarý Mesajý ############
  stopCluster(cl)
  registerDoSEQ()
}



arc.progress_label("Adjusting Weights...")
arc.progress_pos(60)


if(is.null(WeigthList[[1]])==F){
  
  for (i in 1:nlist) {
    carnList[[i]] <- varImp(WeigthList[[i]], scale=FALSE)
  }
  
  orderList <- lapply(1:nlist, function(x) NULL)
  agirlikList <- lapply(1:nlist, function(x) NULL)
  
  for (i in 1:nlist) {
    
    carnList[[i]] <- varImp(WeigthList[[i]], scale=FALSE)
    agirlikList[[i]] <- carnList[[i]][["importance"]][["Overall"]][order(as.numeric(gsub("[^0-9]+", "", row.names(carnList[[i]]$importance))))]
    rankCorrWeigthList[[i]] <- carnList[[i]][["importance"]]
    }
  
  
}else {
  agirlikList <- lapply(1:nlist, function(x) NULL)
  
  for (i in 1:nlist) {
    agirlikList[[i]] <- t(rankCorrWeigthList[[i]])
  }
}

newRastlist <- rfiles1
emreList <- lapply(1:nlist, function(x) NULL)

for (i in 1:nlist) {
  emreList[[i]] <- stack(newRastlist[i])
}

weigthLeng <- lapply(1:nlist, function(x) NULL)


for (j in 1:nlist) {
  weigthLeng[[j]] <-   (length(agirlikList[[j]]))
}

arc.progress_label("Factor Weights are leveling...")
arc.progress_pos(80)

for(i in 1:nlist){
  for (j in 1:weigthLeng[[i]]) {
    values(emreList[[i]])[,j]  <-  values(emreList[[i]])[,j]*agirlikList[[i]][j]
  }
}

rasterList <- lapply(1:nlist, function(x) NULL)
for (i in 1:nlist) {
  rasterList[[i]] <- do.call(calc, c(emreList[[i]], fun = max, na.rm = T))
  
}

arc.progress_label("Raster Data is saving on folder location")
arc.progress_pos(100)

stackData <- stack(rasterList)

writeRaster(x = stackData, filename = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
            else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
            ,overwrite=TRUE)

dosya2 <- strsplit(x=kayitPath, split='[.]')[[1]][1]
dosya2 <- paste0(dosya2,".csv")

names <- (sub('\\..*$', '', basename(as.character(rfiles1))))
write.csv(names,dosya2)


max.rows <- max(unlist(lapply(rankCorrWeigthList, nrow), use.names = F))

list.df <- lapply(rankCorrWeigthList, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

list.weight <- lapply(1:nlist, function(x) NULL)

for (i in 1:nlist) {
  list.weight[[i]] <- setDT(as.data.frame(list.df[[i]]), keep.rownames = TRUE)[]
}
if(length(writeXls)){
  if(!file.exists(writeXls)){
    write.xlsx(list.weight, file = writeXls, col.names = T, row.names = T, append = F)
  }else{
    write.xlsx(list.weight, file = writeXls, col.names = T, row.names = T, append = F)
  }
}


return(out_params)
}


