#####################################################################################################  
### Calculation of Frequency Ratio of Raster Images
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
  arc.progress_label("Loading Libraries...")
  arc.progress_pos(0)
  if (!requireNamespace("rgdal", quietly = TRUE))
    install.packages("rgdal")
  if (!requireNamespace("raster", quietly = TRUE))
    install.packages("raster")
  if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
  if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
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
  if (!requireNamespace("cartography", quietly = TRUE))
    install.packages("cartography")
  
  require(cartography)
  require(rgdal)
  require(raster)
  require(sp)
  require(rgeos)
  require(classInt)
  require(dplyr)
  require(stats)
  require(svDialogs)
  require(xlsx)
  
  rgdal::set_thin_PROJ6_warnings(TRUE)
 
  ### Functionsw
  #function_1
  CrsCheck <- function(crsCodes){
    result <- "continue"
    projCode <- unlist(lapply(crsCodes, function(x){
      pos1 <- strsplit(x, "+proj=")
      pos2 <- pos1[[1]][length(pos1[[1]])]
      pos3 <- strsplit(pos2, "[ +]")[[1]][1]
    }))
    
    for(i in 1:length(projCode)){
      for(j in i:length(projCode)){
        if(!is.na(projCode[i]) & !is.na(projCode[j]) & !is.null(projCode[i]) & !is.null(projCode)){
          if(projCode[i] != projCode[j]){
            result <- dlg_message(c("Girmiþ olduðunuz verilerde Koordinat verisi (CRS) bulunmamakta veya veriler arasýnda koordinat uyuþumsuzluðu vardýr.", "Devam Etmek Ýstiyor musunuz?"), "okcancel")$res
            if(result == "cancel") return("cancel")
          }
        }else{
          result <- dlg_message(c("Girmiþ olduðunuz verilerde Koordinat verisi (CRS) bulunmamakta veya veriler arasýnda koordinat uyuþumsuzluðu vardýr.", "Devam Etmek Ýstiyor musunuz?"), "okcancel")$res
          if(result == "cancel") return("cancel")
        } 
      }
    }
    if(result != "cancel") return("continue")
  }
  
  #function_2
  resoCheck <- function(resoList){
    cevap <- "cevap"
    for(i in 1:length(resoList)){
      for(j in i:length(resoList)){
        if(!isTRUE(all.equal(resoList[[i]],resoList[[j]]))){
          msg_box(" Yüklediðiniz verilerde çözünürlük uyumsuzlugu bulunmaktadir! 
                \n Ýþleminiz durdurulmustur.")
          cevap <- "no"
          return(cevap)
        }
      }
    }
    return(cevap)
  }
  
  #function_3
  extentCheck <- function(extentList){
    cevap <- "cevap"
    for(i in 1:length(extentList)){
      for(j in 1:length(extentList)){
        kesisimKontrol <- intersect(extentList[[i]],extentList[[j]])
        if(is.null(kesisimKontrol)){
          msg_box(" Yüklediðini veriler de pencere boyutu (Extent) uyuþumsuzluðu yaþanmýþtýr. 
                \n Lütfen yüklediðiniz verilerin Pencere Boyutlarýnýn (Extent) ayný olmasýna dikkat ediniz. 
                \n Ýþleminiz durdurulmuþtur.")
          cevap <- "no"
          return(cevap)
        }
      }
    }
    return(cevap)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  #####################################################################################################  
  
  arc.progress_label("Data Reading...")
  arc.progress_pos(20)
  rfiles1 <- in_params[[1]]
  classifierName <- tolower(in_params[[2]])
  classNumber <- in_params[[3]]
  sdNumber <- in_params[[4]]
  landPath <- in_params[[5]]
  rfiles2 <- in_params[[6]]
  writeXls <- out_params[[1]]
  kayitPath <-out_params[[2]]
  
  ##################################################################################################### 
  ### Load Data  ####  
  #####################################################################################################
  landShp <- arc.open(landPath)
  landShp <- arc.data2sp(arc.select(landShp))
  landShp$heyelanTur <- 1
  
  ##################################################################################################### 
  ### Check Crs, Extent, Res Datas  ####  
  #####################################################################################################
 
   arc.progress_label("Checking CRS, Extent and Resolution...")
  arc.progress_pos(30)

  result <- "sonuc"
  clCrsCodes <- unlist(lapply(rfiles1, function(x){
    proj4string(raster(x))
  }))
  uclCrsCodes <- unlist(lapply(rfiles2, function(x){
    proj4string(raster(x))
  }))
  crsCodes <- c(clCrsCodes, uclCrsCodes, proj4string(landShp))
  result <- CrsCheck(crsCodes)
  if(result == "cancel") return(out_params)
  cevap <- "cevap"
  clExtent <- unlist(lapply(rfiles1, function(x){
    extent(raster(x))
  }))
  uclExtent <- unlist(lapply(rfiles2, function(x){
    extent(raster(x))
  }))
  clres <- (lapply(rfiles1, function(x){
    res(raster(x))
  }))
  uclres <- (lapply(rfiles2, function(x){
    res(raster(x))
  }))
  extents <- c(clExtent, uclExtent, extent(landShp))
  resos <- c(clres, uclres)
  cevap <- resoCheck(resos)
  if(cevap == "no") return(out_params)
  cevap <-extentCheck(extents)
  if(cevap == "no") return(out_params)
  
  ##################################################################################################### 
  ### Classifier Frequency Ratio 
  #####################################################################################################
 
  arc.progress_label("Calculating Frequency Ratio for Classified Data...")
  arc.progress_pos(50)
  classRasters <- unlist(lapply(rfiles1, function(x){
  clRaster <- raster(x)

    if(classifierName == "msd"){
      breaks <- getBreaks(v = values(clRaster), k=sdNumber,method="msd", middle = TRUE)
      if (length(breaks)>15){
        breaks <- classIntervals(values(clRaster), n=classNumber,style="quantile",warnLargeN = FALSE)$brks
              }
      breaks <- unique(breaks)
    } else if (classifierName == "fisher"){
      breaks <- classIntervals(sampleRandom(clRaster,1000), n=classNumber,style=classifierName,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    } else if (classifierName == "kmeans"){
      breaks <- classIntervals(values(clRaster), n=classNumber,style=classifierName,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    else{
      breaks <- classIntervals(values(clRaster), n=classNumber,style=classifierName,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    
    
    classRaster <- with(clRaster, cut(clRaster,breaks=breaks, na.rm=TRUE),include.lowest=TRUE)
    
    a <- round(breaks, digits = 7)
    breakpoints <- list()
    for(i in 1:(length(breaks) - 1)){
        breakpoints[[i]] <- paste0(a[[i]], " - ", a[[i+1]])
    }
    
    breakpoints <- unlist(breakpoints)
    dataTable <- as.integer(unlist(as.list(table(values(classRaster)))))
    classData <- data.frame("BP" = breakpoints, "CN" = factor(1:length(dataTable)), "PN" = dataTable[1:length(dataTable)])
    classData$PerPN = (classData$PN / sum(classData$PN))
    
    train1 <- rasterize(landShp,field = "heyelanTur",classRaster, updateValue= "NA")
    predictors <- stack(classRaster,train1)
    value_table <- getValues(predictors)
    value_table <- na.omit(value_table)
    value_table <- as.data.frame(value_table)
   
    ##################################################################################################### 
    ### Correction in case of missing value
    ##################################################################################################### 
    
    landDf <- as.data.frame(table(value_table[[1]]))
    colnames(landDf) <- c("CN", "LN")
    df <- data.frame("CN" = factor(1:length(dataTable)), "LN" = 0)
    df <- rbind(df, landDf) %>% group_by(CN) %>% summarise_each(funs(sum))
    landTable <- as.integer(unlist(as.list(table(value_table[[1]]))))
    classData$LN <- df$LN
    
    classData$PerLN <- classData$LN / sum(classData$LN)
    classData$FR <- round((classData$PerLN / classData$PerPN) , digits = 2)
    deneme <- classRaster
    
    for(i in 1:length(dataTable)){
      values(deneme) <- ifelse(values(deneme) == classData$CN[i], classData$FR[i], values(deneme))
    }
    names(deneme) <- names(clRaster)
    if(length(writeXls)){
      if(!file.exists(writeXls)){
        write.xlsx(classData, file = writeXls, col.names = T, row.names = F, sheetName = names(deneme), append = F)
      }else{
        write.xlsx(classData, file = writeXls, col.names = T, row.names = F, sheetName = names(deneme), append = T)
      }
    }
    deneme
  }))
  
  ##################################################################################################### 
  ### Unclassifier Frequency Ratio
  #####################################################################################################
  
  arc.progress_label("Calculating Frequency Ratio for Unclassified Data...")
  arc.progress_pos(70)
  if(length(rfiles2)){
    classedRasters <- unlist(lapply(rfiles2, function(x){
    classRaster <- raster(x)
    classData <- data.frame(table(values(classRaster)))
    colnames(classData) <- c("CN", "PN")
    classData$PerPN = (classData$PN / sum(classData$PN))
    
    train1 <- rasterize(landShp,field = "heyelanTur",classRaster, updateValue= "NA")
    predictors <- stack(classRaster,train1)
    value_table <- getValues(predictors)
    value_table <- na.omit(value_table)
    value_table <- as.data.frame(value_table)

    landDf <- as.data.frame(table(value_table[[1]]))
    colnames(landDf) <- c("CN", "LN")
    df <- data.frame("CN" = classData$CN, "LN" = 0)
    df <- rbind(df, landDf) %>% group_by(CN) %>% summarise_each(funs(sum))
    landTable <- as.integer(unlist(as.list(table(value_table[[1]]))))
    classData$LN <- df$LN
    
    classData$PerLN <- classData$LN / sum(classData$LN)
    classData$FR <- round((classData$PerLN / classData$PerPN) , digits = 2)
    deneme <- classRaster
    
    for(i in 1:nrow(classData)){
      values(deneme) <- ifelse(values(deneme) == classData$CN[i], classData$FR[i], values(deneme))
    }
    names(deneme) <- names(classRaster)
    if(length(writeXls)){
      if(!file.exists(writeXls)){
        write.xlsx(classData, file = writeXls, col.names = T, row.names = F, sheetName = names(deneme), append = F)
      }else{
        write.xlsx(classData, file = writeXls, col.names = T, row.names = F, sheetName = names(deneme), append = T)
      }
    }
    deneme
  }))
  }
  
  ##################################################################################################### 
  ### Writing Data
  #####################################################################################################
  
  arc.progress_label("Writing Data...")
  arc.progress_pos(90)
  if(length(rfiles2)){
    stackData <- stack(c(classRasters, classedRasters))
  }else{
    stackData <- stack(classRasters)
  }
  
  
  dosya2 <- strsplit(x=kayitPath, split='[.]')[[1]][1]
  dosya2 <- paste0(dosya2,".csv")
  
  writeRaster(x = stackData, filename = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
              else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
              ,overwrite=TRUE)
 
  write.csv(names(stackData),dosya2)
  
  return(out_params)
}  