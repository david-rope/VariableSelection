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

file_train  <- ".../Higgs/DataSets/train_smote_higgs_fold_3.csv"
file_test <- ".../Higgs/DataSets/test_higgs_fold_3.csv"
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

data_train$class <- as.factor(data_train$class)
str(data_train,list.len=ncol(data_train))

data_test$class <- as.factor(data_test$class)
str(data_test,list.len=ncol(data_test))

####################################
##
##  Training models increasing variables
##
####################################
## LOGISTIC marker

lr.model <- glm(formula = class ~
, data = data_train, family="binomial")

predic.train <- predict(lr.model, newdata=data_train, type = "response")
predic.test <- predict(lr.model, newdata=data_test, type = "response")

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)
results <- list(target_train = data_train$class, predic_train = predic.train,
                target_test = data_test$class, predic_test = predic.test, 
                tiempo = total_time)

saveRDS(results,
        ".../Higgs/LR/Fold3/LR_results_higgs_iv_NVAR.rds")
saveRDS(lr.model, ".../Higgs/LR/Fold3/LR_model_higgs_iv_NVAR.rds")
