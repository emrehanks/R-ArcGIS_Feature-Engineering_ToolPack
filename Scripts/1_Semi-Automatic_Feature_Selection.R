#####################################################################################################  
###########            ##############
#####################################################################################################

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
  arc.progress_label("Kütüphaneler Yükleniyor...")
  arc.progress_pos(0)
  if (!requireNamespace("rgdal", quietly = TRUE))
    install.packages("rgdal")
  if (!requireNamespace("raster", quietly = TRUE))
    install.packages("raster")
  if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
  if (!requireNamespace("classInt", quietly = TRUE))
    install.packages("classInt")
  if (!requireNamespace("dplyr", quietly = TRUE))
    install.packages("dplyr")
  if (!requireNamespace("stats", quietly = TRUE))
    install.packages("stats")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  if (!requireNamespace("xlsx", quietly = TRUE))
    install.packages("xlsx")
  if (!requireNamespace("rasterVis", quietly = TRUE))
    install.packages("rasterVis")
  if (!requireNamespace("e1071", quietly = TRUE))
    install.packages("e1071")
  if (!requireNamespace("spatialEco", quietly = TRUE))
    install.packages("spatialEco")
  if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
  if (!requireNamespace("corrplot", quietly = TRUE))
    install.packages("corrplot")
  if (!requireNamespace("gridExtra", quietly = TRUE))
    install.packages("gridExtra")
  if (!requireNamespace("quantreg", quietly = TRUE))
    install.packages("quantreg")
  
  require(gridExtra)
  require(xlsx)
  require(rgeos)
  require(caret)
  require(rasterVis)
  require(corrplot)
  require(e1071)
  require(spatialEco)
  require(rgdal)
  require(raster)
  require(sp)
  require(classInt)
  require(dplyr)
  require(stats)
  require(svDialogs)
  require(quantreg)
  
  #source("C:/Users/Public/kullanilanFonksiyonlar.R")
  rgdal::set_thin_PROJ6_warnings(TRUE)
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  #####################################################################################################  
  arc.progress_label("Data Reading...")
  arc.progress_pos(20)

  rfiles1 <- in_params[[1]]
  csvPath <- in_params[[2]]
  trainPath <- in_params[[3]]
  typeSkewwness <- in_params[[4]]
  corrMethod <- tolower(in_params[[5]])
  splitCheck <- in_params[[6]]
  transformType <- tolower(in_params[[7]])
  kayitPath <- in_params[[8]]
  
  #yazilan fonksiyonlarin uzantilari
  #source("C:/Users/Public/kullanilanFonksiyonlar.R")
  
  stackNames<-read.csv(csvPath)
  train <- raster(trainPath)
  rasters1 <- brick(rfiles1)
  nlist <- rasters1@data@nlayers
  stackNames<-read.csv(csvPath)
  names(rasters1) <- stackNames[[2]]
  
  ### necessary functions
  
  dfFeature <- function(featureRaster){
    featureDF=getValues(featureRaster)
    featureDF=na.omit(featureDF)
    featureDF=as.data.frame(featureDF)
    return(featureDF)
  }
  
  FeatureData <- function(features,train){
    
    #train <- resample(train,features, resample='bilinear')
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
  
  rounded_value <- function(value) {
    value <- round(value,digits = 0)
    return (value)
  }
  
  createSets <- function(x, y, p){
    nr <- NROW(x)
    size <- (p) %/% length(unique(y))
    idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size))
    unlist(idx)
    
  }
  # converts raster data to dataFrame
  featureDF <- dfFeature(rasters1)
  
  # calculate correlation matrix
  

  corMethod <- corrMethod
  correlationMatrix <- cor(featureDF[,1:length(featureDF)], method = corMethod)
  correlationMatrix <- round(correlationMatrix,digits = 5)
  corrplot(correlationMatrix, method = "number")
  setwd(kayitPath)
  
  for(i in (1:1)) {
    tiff("corrA.tif", width = 1920, height = 1080, res = 150)
    corrplot(correlationMatrix, method = "number")
    Sys.sleep(1)
    ans = winDialog(type = c("ok"), "Have You Identified Highly Correlated Data?")
    dev.off()
  }
  
    #separating data that cannot be select such as curvature
  txt <- names(rasters1)
  txt <- select.list(txt, preselect = NULL, multiple = TRUE,
                     title = "High Remove corr. variables", graphics = T)
  if(length(txt)){
    for(i in 1:length(txt)){
      a <- grep(txt[i], names(rasters1))
      if(length(a)){
        rasters1 <- dropLayer(rasters1,a)
        print(cat(paste(txt[i]),"Faktör Seti Ýçerisinden Çýkarýlmýþtýr"))
      }
    }
  }
  cat("Remaining Number of Factors :", paste(length(rasters1@layers)))
 
   #converts raster data to dataFrame
  featureDF <- dfFeature(rasters1)
  
  
  # calculate correlation matrix
  corMethod <- corrMethod
  correlationMatrix <- cor(featureDF[,1:length(featureDF)], method = corMethod)
  
  for(i in (1:1)) {
    tiff("corrB.tif", width = 1920, height = 1080, res = 150)
    corrplot(correlationMatrix, method = "number")
    Sys.sleep(1)
    ans = winDialog(type = c("ok"), "Have You Analyzed New Correlation Graph?")
    dev.off()
  }
  
  type <- "samplingRatio"
  value <- 70
  
  ##################################################################################################### 
  ### Create Training and Testing Datasets  ####  Eðitim Test Verisinin Oluþturulmasý 
  #####################################################################################################
  arc.progress_label("Preparing DataSet...")
  arc.progress_pos(40)
  #featurelerin tutuldugu raster veri ile train verisinin rasterini resample yapýp egitim ve test olarak ayirmak icin hazir hale getirir
  valueDF <- FeatureData(rasters1,train)
  
  ### function of train/test praperation

  TrainTestSplit <- function(value_table,type = "Örneklem Oraný",value = 70){
    
    if(type == "samplingRatio"|type =="Sample Ratio"){
      if(value > 95){
        cat("The percentile cannot be more than 95")
        value = 95
      }
      else if(value < 5){
        cat("Percentile value cannot be less than 5%.")
        value =5
      }
      
      maxverisayisi <- min(table(value_table$train)) * 2
      trainsayisi <- as.integer(maxverisayisi*value/100) 
      testsayisi <- maxverisayisi - trainsayisi
      trainid <- createSets(value_table,value_table$train,trainsayisi)
      testid <- createSets(value_table,value_table$train,testsayisi)
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
      
    }}
  
  #verilen type ve value degerine gore train ve test verisi ayrimi yapar
  trainTestDf <- TrainTestSplit(value_table = valueDF,type = "samplingRatio", value = 70)
  #train ve test verisinin degiskenlere atanmasi
  traindata <- trainTestDf$train
  testdata <- trainTestDf$test
  
  #####################################################################################################
  ### Fit Model ### Modelin Eðitilmesi
  #####################################################################################################
  arc.progress_label("Model Building...")
  arc.progress_pos(60)
  #linear regression egitimi

  linRegFit <- lm(train ~., data = traindata)
  sumLin <- summary(linRegFit)
  sumList <- sumLin[["coefficients"]][,4]
 
  print(cat("Calculated RSquare(R2) Value:", paste(round(sumLin$r.squared,digits = 4)),""))
  
  pearsonValues <- round(sumList,digits = 5)
  pearsonValues <-pearsonValues[-1]
  nlist <- length(pearsonValues)
  
  #########  Araligi Yazdýrma
  intervalList <- lapply(1:nlist, function(x) NULL)
  
  for (i in 1:nlist) {
    if (pearsonValues[i] > 0.05){
      #print("not statistically significant")
      intervalList[i] <- c("not statistically significant")
      
    } else {
      #print ("Statistically significant")
      intervalList[i] <- c("Statistically significant")
    }
  }
  pearsonValues <- as.list(pearsonValues)
  aa <- cbind(pearsonValues,intervalList)
  w <- as.data.frame(which(pearsonValues > 0.05, arr.ind = TRUE))
  w <- as.data.frame(w)
  print(aa)
  
  wlist <- length(rownames(w))
  ada <- cbind(rownames(w))
  new.names<-NULL
  
  
  ##### remove high correlated factor in raster database
  if (wlist==0) {
    data <- rasters1
    answer <- 'NO'
  } else {
    for(i in 1:wlist){
      new.names[i]<-paste(ada[i,],sep=)
      answer<-winDialog("yesno", paste(print(new.names[i])," factor is irrelevant, remove from the dataset?"))
    }
        }
  if (answer=='YES') {
    data <- rasters1
    if(length(rownames(w))){
      for(i in 1:length(rownames(w))){
        a <- grep(rownames(w)[i], names(data))
        if(length(a)){
          data <- dropLayer(data,a)
          print(cat(paste(rownames(w)[i]),"was removed from the factor dataSet"))
        }
      }
    }
    
  } else {
    data <- rasters1
    answer <- 'NO'
    }
  
  featureDF <- dfFeature(data)
  # calculate correlation matrix
  correlationMatrix <- cor(featureDF[,1:length(featureDF)], method = corMethod)
  corrplot(correlationMatrix, method = "number")
  
  ############### SKEWNESS  ################
  nlist <- length(data@layers)
  skewList <- lapply(1:nlist, function(x) NULL)
  allrasters <- data
  
  #cat("The skewness of landslide related factors duration are :")
    nameList <- lapply(1:nlist, function(x) NULL)
  
  if (typeSkewwness=="Type_1") {
    typeSkewwness <- 1
  
  } else if (typeSkewwness=="Type_2"){
    typeSkewwness <- 2
  } else { typeSkewwness <- 3
  }
  for (i in 1:nlist) {
    skewList[[i]] <- skewness(as.double(values(allrasters[[i]])), na.rm = T, type = typeSkewwness)
    nameList[i] <- sub('\\..*$', '', names(data[[i]]))
    #print(skewList[[i]])
  }
  
  nameValueStock <- data.frame(cbind(nameList,skewList))

  hist(allrasters)
  
  ######### Writing of Skewness Range
  new.list<-NULL
  
  intervalList <- lapply(1:nlist, function(x) c("symmetric"))

    
    for (i in 1:nlist) { 
    if (skewList[i] < -1 || skewList[i] > 1) {
     # print("highly skewed")
      intervalList[i] <- c("highly skewed")
    }
    else if (skewList[i] <= -1 & skewList[i] >= -0.5)
    {
      #print("moderate skewed")
      intervalList[i] <- c("moderate skewed")
    }
    else if (skewList[i] <= 1 & skewList[i] >= 0.5)
    {
      #print("moderate skewed")
      intervalList[i] <- c("moderate skewed")
    }
    else if (skewList[i] == "NULL")
    {
      #print("symmetric")
      intervalList[i] <- c("symmetric")
    }
    else #print("symmetric")
    intervalList[[i]] <- c("symmetric")
  
  new.list[i] <- paste(intervalList[i],sep="")
  }
  new.list
  
  rownames(nameValueStock) <- nameValueStock[[1]]
  print(cbind(nameValueStock[2],new.list))
  
  s <- which(nameValueStock[2] > 1, arr.ind = TRUE)
  a <- rownames(s)
  ############### Log Data  ################
  ############## option 1 ############
  cat("The skewness values of factors after processing are :")
  if(length(rownames(s))){
    for(i in 1:length(rownames(s))){
      dataLod <- log10(as.data.frame(values(data[[s[i]]]))+1-min(as.data.frame(values(data[[s[i]]])),na.rm = T))
      if(length(a)){
        values(data[[s[i]]]) <- as.matrix(dataLod)
        print(cat(paste(rownames(s)[i])," factor was normally distributed using Log Transformation..."))
      }
    }
  }
 
   nameList <- lapply(1:nlist, function(x) NULL)
  for (i in 1:nlist) {
    skewList[[i]] <- skewness(as.double(values(data[[i]])), na.rm = T, type = 3)
    nameList[i] <- names(data)[i]
  
  }

     ############ Log Skewness Name ##########
  nameValueStock <- data.frame(cbind(nameList,skewList))
  nameValueStock <- data.frame(cbind(nameValueStock,intervalList[1]))
  print(nameValueStock)
  ##### histogram of the skewed data

    ######### Statistical transformation for rasters using with spatialEco Package ##### raster.transformation {spatialEco}

  if(length(splitCheck)){
    if(splitCheck){
      transRaster <- raster.transformation(data, trans = transformType, smin = 0, smax = 255)
      lapply(1:nlayers(transRaster), function(x){
        singleRaster <- raster(transRaster,layer = x)
        dosya <- paste0(kayitPath,"/",names(singleRaster),".","tif",sep="")
        arc.write(data =singleRaster, path= if(grepl("\\.tif$", dosya)| grepl("\\.img$", dosya)) dosya
                    else paste0(normalizePath(dirname(dosya)),"\\", sub('\\..*$', '', basename(dosya)),".tif")
                    ,overwrite=TRUE)
      })
    }
    else {
      lapply(1:nlayers(data), function(x){
        singleRaster <- raster(data,layer = x)
        dosya <- paste0(kayitPath,"/",names(singleRaster),".","tif",sep="")
        arc.write(data =singleRaster, path= if(grepl("\\.tif$", dosya)| grepl("\\.img$", dosya)) dosya
                    else paste0(normalizePath(dirname(dosya)),"\\", sub('\\..*$', '', basename(dosya)),".tif")
                    ,overwrite=TRUE)
      })
    }
    
    }
  
  
  ##################################################################################################### 
  ### Raster Stack Data to Single Band Raster and Write Out  ####  
  #####################################################################################################
  for(i in (1:1)) {
    tiff("histB.tif", width = 1920, height = 1080, res = 150)
    hist(data)
    Sys.sleep(1)
    ans = winDialog(type = c("ok"), "Have You Analyzed New Correlation Graph?")
    dev.off()
  }

  return(out_params)
}


