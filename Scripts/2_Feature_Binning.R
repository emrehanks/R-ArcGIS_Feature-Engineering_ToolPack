
#####################################################################################################  
###########   DETAILS of CODES AND ADDITIONAL INFORMATION             ##############
#####################################################################################################

  tool_exec <- function(in_params, out_params)
  {
  #####################################################################################################  
  ### Check/Load Required Packages  ####  
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
  if (!requireNamespace("classInt", quietly = TRUE))
    install.packages("classInt")
  if (!requireNamespace("dplyr", quietly = TRUE))
    install.packages("dplyr")
  if (!requireNamespace("stats", quietly = TRUE))
    install.packages("stats")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  
  require(rgdal)
  require(raster)
  require(sp)
  require(classInt)
  require(dplyr)
  require(stats)
  require(svDialogs)
  

  ### functions used in this module
  
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
            result <- dlg_message(c("There is no coordinate factor (CRS) in the dataset or there is a coordinate mismatch.", "Do you want to continue the process?"), "okcancel")$res
            if(result == "cancel") return("cancel")
          }
        }else{
          result <- dlg_message(c("There is no coordinate factor (CRS) in the dataset or there is a coordinate mismatch.", "Do you want to continue the process?"), "okcancel")$res
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
          msg_box(" There is a resolution incompatibility in the uploaded dataset! 
                \n process is halt.")
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
          msg_box(" A problem encountered about window size (Extent) mismatch. 
                \n Please make sure the extent size of the uploaded data is to the same extent. 
                \n process is halt.")
          cevap <- "no"
          return(cevap)
        }
      }
    }
    return(cevap)
  }
  
  #function_4
    funclasifier <- function(x,y = "quantile",n = 5){
    if(y == "fisher"){
      breaks <- classIntervals(sampleRandom(x,1000), n=n,style=y,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    else if (y=="kmeans") {
      breaks <- classIntervals(values(x), n=n,style=y,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    
    else{
      breaks <- classIntervals(values(x), n=n,style=y,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    
    with(x, cut(x,breaks=breaks, na.rm=TRUE),include.lowest=TRUE)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################  
  
  arc.progress_label("Data Reading...")
  arc.progress_pos(20)

  rfiles1 <- in_params[[1]]
  classifierName <- tolower(in_params[[2]])
  classNumber <- in_params[[3]]
  splitCheck <- in_params[[4]]
  rfiles2 <- in_params[[5]]
  kayitPath <- in_params[[6]]
  kayitPath2 <- in_params[[7]]

  result <- "sonuc"
  clCrsCodes <- unlist(lapply(rfiles1, function(x){
    proj4string(raster(x))
  }))
  uclCrsCodes <- unlist(lapply(rfiles2, function(x){
    proj4string(raster(x))
  }))
  crsCodes <- c(clCrsCodes, uclCrsCodes)
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
  extents <- c(clExtent, uclExtent)
  resos <- c(clres, uclres)
  cevap <- resoCheck(resos)
  if(cevap == "no") return(out_params)
  cevap <-extentCheck(extents)
  if(cevap == "no") return(out_params)

  splitRas <- lapply(rfiles1, function(x){
  arc.progress_label(paste0(basename(x), " data is classified..."))
   
  ##################################################################################################### 
  ### Load Data  
  #####################################################################################################
   
  classRaster <- raster(x)
  classRaster <- funclasifier(x = classRaster, y = classifierName, n = classNumber)
    arc.progress_label(paste0(basename(x), " classified data is writing..."))
    writeRaster(x = classRaster, filename = paste0(kayitPath,"\\sinif",basename(x))
                ,overwrite=TRUE)
    classSing <- list()
    if(length(splitCheck)){
      if(splitCheck){
      a <- data.frame(table(values(classRaster)))
        for(i in 1:nrow(a)){
          editRas <- classRaster
          values(editRas) <- ifelse(values(editRas) == as.numeric(a$Var1[i]), 1, 0)
          classSing[[i]] <- editRas
          
        }
      }
    }
    
    classSing
    stackRas <- stack(classSing) 
    writeRaster(x = stackRas, filename = paste0(kayitPath2,"\\split",basename(x)),overwrite=TRUE)
  })
  
  if(length(splitCheck)){
    if(splitCheck){
      if(length(rfiles2)){
        splitRas2 <- lapply(rfiles2, function(x){
          arc.progress_label(paste0(basename(x), " splitting process is applying to the file..."))
          classRaster <- raster(x)
          writeRaster(x = classRaster, filename = paste0(kayitPath,"\\sinif",basename(x)),overwrite=TRUE)
          classSing <- list()
          a <- data.frame(table(values(classRaster)))
          for(i in 1:nrow(a)){
            editRas <- classRaster
            values(editRas) <- ifelse(values(editRas) == as.numeric(a$Var1[i]), 1, 0)
            classSing[[i]] <- editRas
          }
          classSing
          stackRas <- stack(classSing) 
          writeRaster(x = stackRas, filename = paste0(kayitPath2,"\\split",basename(x)),overwrite=TRUE)
        })
        splitRas2 <- unlist(splitRas2)
        splitRas <- unlist(splitRas)
        splitRas <- stack(c(splitRas, splitRas2))
        writeRaster(x = splitRas, filename = paste0(kayitPath2,"\\SplitStackData.tif"), format="GTiff", overwrite = T)
      }else{
        splitRas <- unlist(splitRas)
        splitRas <- stack(splitRas)
        writeRaster(x = splitRas, filename = paste0(kayitPath2,"\\SplitStackData.tif"), format="GTiff", overwrite = T)
      }
    }
  }
  
  return(out_params)
}