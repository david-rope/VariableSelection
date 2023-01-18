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

####################################
##
##  Load data
##  
####################################

file_train  <- "/data1/David/BashR/KFoldsCV/Bank/DataSets/train_smote_bank_fold_5.csv"
file_test <- "/data1/David/BashR/KFoldsCV/Bank/DataSets/test_bank_fold_5.csv"
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

data_train$y <- as.factor(data_train$y)
str(data_train,list.len=ncol(data_train))

data_test$y <- as.factor(data_test$y)
str(data_test,list.len=ncol(data_test))

####################################
##
##  Training models increasing variables
##
####################################
## LOGISTIC marker

lr.model <- glm(y~
, data = data_train, family="binomial")

predic.train <- predict(lr.model, newdata=data_train, type = "response")
predic.test <- predict(lr.model, newdata=data_test, type = "response")

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)
results <- list(target_train = data_train$y, predic_train = predic.train,
                target_test = data_test$y, predic_test = predic.test, 
                tiempo = total_time)

saveRDS(results,
        "/data1/David/OutputR/KFoldsCV/Bank/LR/Fold5/LR_results_bank_iv_NVAR.rds")
saveRDS(lr.model, "/data1/David/OutputR/KFoldsCV/Bank/LR/Fold5/LR_model_bank_iv_NVAR.rds")

