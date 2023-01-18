####################################
##
## Ranking using Information Value
##
####################################
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(cvAUC)
###########################
##
## Variable selection, mass package, but you can use leaps package 
##
###########
library(MASS)
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
  ## See data structure and change to factor variables as necessary
  ####################################
  
  #  str(data_train,list.len=ncol(data_train))
  data_train$target <- as.factor(data_train$target)
  #  str(data_train,list.len=ncol(data_train))
  
  ####################################
  ##
  ##  Built the BEM model
  ##
  ####################################

  full.model <- glm(target ~., data = data_train, family="binomial")
  backward.model <- stepAIC(full.model, direction = "backward", trace = FALSE)

  terms <- attributes(backward.model$terms)[3]
  nvar<- length(terms$term.labels)
  print(nvar)

  predic_train <- predict(backward.model, newdata=data_train, type = "response")
  aucValueTrain<-AUC(predic_train, data_train$target)
  print(aucValueTrain)
    
  model_name <- paste(".../Heart/BEM/stepwise_model_heart_fold_", i, sep = "")
  model_name <- paste(model_name, ".rds", sep = "")                                
  print(model_name)
  
  saveRDS(backward.model, model_name)

}

#           Nvar      AUC
# Fold 1     11    0.9322199
# Fold 2     10    0.93882
# Fold 3      9    0.931933
# Fold 4     10    0.927112
# Fold 5      7    0.9261938



