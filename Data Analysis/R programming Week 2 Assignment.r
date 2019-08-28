
pollutantmean <- function(Directory, pollutant, id = 1:332) {
  
  setwd("D:/Project/R Programming")
  path <- paste(getwd(),"/", "specdata" , sep = "")

  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      temp <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), as.is = T, header = T)
      data <- rbind(data,temp)
    }
    else if (i < 100) {
      temp <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), as.is = T, header = T)
      data <- rbind(data,temp)
    }
    else {
      temp <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), as.is = T, header = T)
      data <- rbind(data,temp)
    }
  }
  return(mean(data[,pollutant], na.rm = T))
}

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")

complete <- function(directory, id = 1:332) {
    
  setwd("D:/Project/R Programming")
  path <- paste(getwd(),"/", "specdata" , sep = "")
    
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      temp <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), as.is = T, header = T)
      
    }
    else if (i < 100) {
      temp <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), as.is = T, header = T)
     
    }
    else {
      temp <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), as.is = T, header = T)
      
    }
      
    nobs <- sum(complete.cases(temp))
    temp_data <- data.frame(i, nobs)
    data <- rbind(data,temp_data)
    
  }
  return(data)
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

corr <- function(directory, threshold = 0) {
    
  setwd("D:/Project/R Programming")
  path <- paste(getwd(), "/", "specdata", sep = "")
    
  corr_vector <- NULL
  for (i in 1:332) {
    if (i < 10) {
      temp <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), as.is = T, header = T)
    }
    else if (i < 100) {
      temp <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), as.is = T, header = T)
    }
    else {
      temp <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), as.is = T, header = T)
    }
    
    data <- temp[complete.cases(temp),]
    if (nrow(data) > threshold) {
      corr_vector <- c(corr_vector, cor(data[,"sulfate"], data[, "nitrate"]))
    }
  }
    
    return(corr_vector)
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
