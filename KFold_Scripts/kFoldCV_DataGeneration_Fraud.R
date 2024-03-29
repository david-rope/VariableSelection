####################################
##
## This script makes k-folds data set,
## the fold number is selected with the 
## 'kfolds' variable.
## The default value is 5.
## Then, the train sample is balance 
## using the SMOTE method.
##
####################################
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)

####################################
##
##  Load data, in this case the test data set do not have the target variable
##  then, we will only consider the train sample and split it into a new train and 
##  test samples.
##  
####################################

file_train  <- ".../CardFraud/creditcard.csv"
data_train <- read.csv(file_train, na = c("", "NA","?"))

nrow(data_train)
## Delete candidates with missing information
## TODO: create a different model 
missing_data_train <-data_train[!complete.cases(data_train),]
nrow(missing_data_train)
###############

data_train <- na.omit(data_train)
nrow(data_train)

#########
## Splitting into the two classes
## we split 20% for train and 
## 80% test, also for k-folds cross validation
#########

nofraud <- data_train[data_train$Class == "0", ]
fraud <- data_train[data_train$Class == "1", ]
nrow(data_train[data_train$Class == "0", ])
nrow(data_train[data_train$Class == "1", ])

#####################################
##
##  Here, we do the k-folds data sets
##  We do not use the SMOTE technique for 
##  the test data set, we merge the two classes
##
#####################################
set.seed(10)
kfolds <- 5
nofraud_folds <- split(nofraud, sample(rep(1:kfolds)))
fraud_folds <- split(fraud, sample(rep(1:kfolds)))

nf <- c(1:kfolds)
for (i in 1:kfolds) {
   class_train_0 <- paste("df_nofraud_train_fold_", i, sep = "")
   class_train_1 <- paste("df_fraud_train_fold_", i, sep = "")
   class_test_0 <- paste("df_nofraud_test_fold_", i, sep = "")
   class_test_1 <- paste("df_fraud_test_fold_", i, sep = "")
   # test sample -> 20%
   new_df_test_0 <- assign(class_test_0, nofraud_folds[[i]])
   new_df_test_1 <- assign(class_test_1, fraud_folds[[i]])
   new_df_test <- rbind(new_df_test_0, new_df_test_1)
   df_test <- paste("df_test_fold_", i, sep = "")
   assign(df_test, new_df_test)
   # train sample -> 80%
   new_df_train_0 <- data.frame()
   new_df_train_1 <- data.frame()
   for (j in nf[-i]){
     new_df_train_0 <- rbind(new_df_train_0, nofraud_folds[[j]]) 
     new_df_train_1 <- rbind(new_df_train_1, fraud_folds[[j]]) 
   }
   new_df_train_0 <- assign(class_train_0, new_df_train_0) 
   new_df_train_1 <-assign(class_train_1, new_df_train_1) 
   new_df_train <- rbind(new_df_train_0, new_df_train_1)
   df_train <- paste("df_train_fold_", i, sep = "")
   assign(df_train, new_df_train)
}

#####################################
##
##  Here, we apply the SMOTE method
##  for the train data set  
##
#####################################
library(smotefamily)

for (i in 1:kfolds) {
   train_name <- paste("df_train_fold_", i, sep = "")
   train_name_base <- as.name(train_name)
   df_train_base <- data.frame(eval(train_name_base))
   n_class_0 <- nrow(df_train_base[df_train_base$Class == "0", ])
   n_class_1 <- nrow(df_train_base[df_train_base$Class == "1", ])
   data_smote <- data.frame()
   if (n_class_0 < n_class_1){
      ds <- as.integer(n_class_1/n_class_0)
      data_smote <- SMOTE(df_train_base, target = df_train_base$Class,
                          K = 5, dup_size = ds)
      data_train_syn <- data_smote$syn_data[sample(nrow(data_smote$syn_data),
                                    n_class_1 - n_class_0), ]
   }else{
      ds <- as.integer(n_class_0/n_class_1)
      data_smote <- SMOTE(df_train_base, target = df_train_base$Class,
                          K = 5, dup_size = ds)
      data_train_syn <- data_smote$syn_data[sample(nrow(data_smote$syn_data),
                                                   n_class_0 - n_class_1), ]
   }
   new_data_train <- rbind(select(data_train_syn,-class), df_train_base)
   smote_name <- paste("df_train_smote_fold_", i, sep = "")
   assign(smote_name, new_data_train)
}

####################################
## Randomly candidates to save the new files
####################################

for (i in 1:kfolds) {
   test_name <- paste("df_test_fold_", i, sep = "")
   train_name <- paste("df_train_smote_fold_", i, sep = "")
   test_name_save <- as.name(test_name)
   train_name_save <- as.name(train_name)
   df_test_output <- data.frame(eval(test_name_save))
   df_train_output <- data.frame(eval(train_name_save))
   df_test_output <- df_test_output[sample(nrow(df_test_output), nrow(df_test_output)), ]
   df_train_output <- df_train_output[sample(nrow(df_train_output), nrow(df_train_output)), ]
   
   fileOuputTest <- paste(".../Fraud/DataSets/test_fraud_fold_", i, sep = "")
   fileOuputTest <- paste(fileOuputTest, ".csv", sep = "")
   fileOuputTrain <- paste(".../Fraud/DataSets/train_smote_fraud_fold_", i, sep = "")
   fileOuputTrain <- paste(fileOuputTrain, ".csv", sep = "")
   write.csv(df_test_output, file=fileOuputTest, row.names = FALSE)
   write.csv(df_train_output, file=fileOuputTrain, row.names = FALSE)
}


