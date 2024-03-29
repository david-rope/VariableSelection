####################################
##
## Ranking using Information Value
## ROC is not possible to draw due to 
## the test sample do not have target class
## We only try to see the training behavior
##
####################################
start_time <- Sys.time()
library(dplyr)
library(C50)

####################################
##
##  Load data
##  
####################################

file_train  <- ".../Fraud/DataSets/train_smote_fraud_fold_5.csv"
file_test <- ".../Fraud/DataSets/test_fraud_fold_5.csv"
data_train <- read.csv(file_train, na = c("", "NA","?"))
data_test <- read.csv(file_test, na = c("", "NA","?"))

nrow(data_train)
nrow(data_test)
## Delete candidates with missing information
## TODO: create a different model 
missing_data_train <-data_train[!complete.cases(data_train),]
missing_data_test <- data_test[!complete.cases(data_test),]
nrow(missing_data_train)
nrow(missing_data_test)
###############

data_train <- na.omit(data_train)
data_test <- na.omit(data_test)
nrow(data_train)
nrow(data_test)

####################################
## See data structure and change to factor variables as necessary
####################################

data_train$Class <- as.factor(data_train$Class)
str(data_train,list.len=ncol(data_train))

data_test$Class <- as.factor(data_test$Class)
str(data_test,list.len=ncol(data_test))

####################################
##
##  Training models increasing variables
##
####################################
## LOGISTIC marker

dt.model <- C5.0.formula(Class ~
, data = data_train)

predic.train <- predict(dt.model, newdata=data_train, type = "prob")
predic.test <- predict(dt.model, newdata=data_test, type = "prob")

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)
results <- list(target_train = data_train$Class, predic_train = predic.train,
                target_test = data_test$Class, predic_test = predic.test, 
                tiempo = total_time)

saveRDS(results,
        ".../Fraud/DT/Fold5/DT_results_fraud_iv_NVAR.rds")
saveRDS(dt.model, ".../Fraud/DT/Fold5/DT_model_fraud_iv_NVAR.rds")
