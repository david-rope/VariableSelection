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
  file_name <- paste("/home/david/R/PaperR/TestData/KFoldsCVSMOTE/Fraud/DataSets/train_smote_fraud_fold_", i, sep = "")
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
  data_train$Class <- as.factor(data_train$Class)
  #  str(data_train,list.len=ncol(data_train))
  
  ####################################
  ##
  ##  Built the BEM model
  ##
  ####################################

  full.model <- glm(Class ~., data = data_train, family="binomial")
  backward.model <- stepAIC(full.model, direction = "backward", trace = FALSE)

  terms <- attributes(backward.model$terms)[3]
  nvar<- length(terms$term.labels)
  print(nvar)

  predic_train <- predict(backward.model, newdata=data_train, type = "response")
  aucValueTrain<-AUC(predic_train, data_train$Class)
  print(aucValueTrain)
    
  model_name <- paste("/home/david/R/PaperR/TestData/KFoldsCVSMOTE/Fraud/BEM/stepwise_model_fraud_fold_", i, sep = "")
  model_name <- paste(model_name, ".rds", sep = "")                                
  print(model_name)
  
  saveRDS(backward.model, model_name)
}

#           Nvar      AUC
# Fold 1     29     0.9970249     
# Fold 2     30     0.998279     
# Fold 3     30     0.9975251       
# Fold 4     28     0.9975678    
# Fold 5     29     0.9981895      



