####################################
##
## Ranking using Information Value
##
####################################
library(dplyr)
library(ggplot2)
library(woeBinning)

####################################
##
##  Load data
##
####################################

kfolds <- 5
totalIV <- matrix()
for (i in 1:kfolds) { 
  file_name <- paste(".../Heart/DataSets/train_smote_heart_fold_", i, sep = "")
  file_name <- paste(file_name, ".csv", sep = "")                                
  print(file_name)
  data_train <- read.csv(file_name, na = c("", "NA","?"))
#  View(data_train)
  nrow(data_train)
  ## Delete candidates with missing information
  ## TODO: create a different model 
  missing_data_train <-data_train[!complete.cases(data_train),]
  nrow(missing_data_train)
  ###############
  
  data_train <- na.omit(data_train)
  nrow(data_train)
  
  ####################################
  ##
  ##  Compute information value per variable
  ##
  ####################################
  plist<-colnames(data_train)
  plist<-plist[plist != "target"];

  vectorIV<-vector()
  ivlist<-vector()
  
  for(j in plist) {       # for-loop over columns
    print(j) 
    resultWOE <- woe.binning(data_train,'target', j, 
                           min.perc.total=0.05, min.perc.class=0.01,
                           stop.limit=0.1, event.class='1')
    vectorIV <- c(vectorIV, unname(resultWOE[[3]]))
    ivlist <- c(ivlist, j)
    Sys.sleep(1)
  }
  vectorIV <- as.numeric(format(round(vectorIV, 8), nsmall = 8))
  resultsIV <- matrix(c(ivlist, vectorIV), ncol = 2)
  resultsIV <- resultsIV[order(resultsIV[,2], decreasing = TRUE),]
  if (i == 1){
    totalIV <- resultsIV
  }else{
    totalIV <- cbind(totalIV, resultsIV)  
  }
}

for (i in 1:kfolds) { 
  fileOutputIV <- paste(".../Heart/IVRank/IV_heart_fold_", i, sep = "")
  fileOutputIV <- paste(fileOutputIV, ".txt", sep = "")   
  write.table(totalIV[,2*i-1], file=fileOutputIV, col.names = FALSE, row.names = FALSE, quote = FALSE)
}
