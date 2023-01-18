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
library(e1071)

####################################
##
##  Load data
##  
####################################

file_train  <- "/data1/David/BashR/KFoldsCV/Pulsar/DataSets/train_smote_pulsar_fold_5.csv"
file_test <- "/data1/David/BashR/KFoldsCV/Pulsar/DataSets/test_pulsar_fold_5.csv"
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

data_train$target_class <- as.factor(data_train$target_class)
str(data_train,list.len=ncol(data_train))

data_test$target_class <- as.factor(data_test$target_class)
str(data_test,list.len=ncol(data_test))

####################################
##
##  Training models increasing variables
##
####################################
## LOGISTIC marker

 svm.model <- svm(target_class ~
, data = data_train, kernel="radial", probability = TRUE)

predic.train <- predict(svm.model, newdata=data_train, probability = TRUE)
predic.test <- predict(svm.model, newdata=data_test, probability = TRUE)

result.train<-as.data.frame(attr(predic.train, "probabilities"))
result.test<-as.data.frame(attr(predic.test, "probabilities"))

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)
results <- list(target_train = data_train$target_class, predic_train = result.train,
                target_test = data_test$target_class, predic_test = result.test, 
                tiempo = total_time)

saveRDS(results,
        "/data1/David/OutputR/KFoldsCV/Pulsar/SVM/Fold5/SVM_results_pulsar_iv_NVAR.rds")
saveRDS(svm.model, "/data1/David/OutputR/KFoldsCV/Pulsar/SVM/Fold5/SVM_model_pulsar_iv_NVAR.rds")
