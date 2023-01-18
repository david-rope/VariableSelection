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

file_train  <- ".../Higgs/data/train.csv"
file_test <- ".../Higgs/data/test.csv"
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

#########
## Splitting into the two classes
## we split 20% for train and 
## 80% test, also for k-folds cross validation
#########

nohiggs <- data_train[data_train$class == "0", ]
higgs <- data_train[data_train$class == "1", ]
nrow(data_train[data_train$class == "0", ])
nrow(data_train[data_train$class == "1", ])

#####################################
##
##  Here, we do the k-folds data sets
##  We do not use the SMOTE technique for 
##  the test data set, we merge the two classes
##
#####################################
set.seed(10)
kfolds <- 5
nohiggs_folds <- split(nohiggs, sample(rep(1:kfolds)))
higgs_folds <- split(higgs, sample(rep(1:kfolds)))

nf <- c(1:kfolds)
for (i in 1:kfolds) {
   class_train_0 <- paste("df_nohiggs_train_fold_", i, sep = "")
   class_train_1 <- paste("df_higgs_train_fold_", i, sep = "")
   class_test_0 <- paste("df_nohiggs_test_fold_", i, sep = "")
   class_test_1 <- paste("df_higgs_test_fold_", i, sep = "")
   # test sample -> 20%
   new_df_test_0 <- assign(class_test_0, nohiggs_folds[[i]])
   new_df_test_1 <- assign(class_test_1, higgs_folds[[i]])
   new_df_test <- rbind(new_df_test_0, new_df_test_1)
   df_test <- paste("df_test_fold_", i, sep = "")
   assign(df_test, new_df_test)
   # train sample -> 80%
   new_df_train_0 <- data.frame()
   new_df_train_1 <- data.frame()
   for (j in nf[-i]){
     new_df_train_0 <- rbind(new_df_train_0, nohiggs_folds[[j]]) 
     new_df_train_1 <- rbind(new_df_train_1, higgs_folds[[j]]) 
   }
   new_df_train_0 <- assign(class_train_0, new_df_train_0) 
   new_df_train_1 <-assign(class_train_1, new_df_train_1) 
   new_df_train <- rbind(new_df_train_0, new_df_train_1)
   df_train <- paste("df_train_fold_", i, sep = "")
   assign(df_train, new_df_train)
}

nrow(new_df_train_0[new_df_test_0$class == "0", ])
nrow(new_df_train_1[new_df_test_1$class == "1", ])
nrow(new_df_test_0[new_df_test_0$class == "0", ])
nrow(new_df_test_1[new_df_test_1$class == "1", ])


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
   n_class_0 <- nrow(df_train_base[df_train_base$class == "0", ])
   n_class_1 <- nrow(df_train_base[df_train_base$class == "1", ])
   data_smote <- data.frame()
   if (n_class_0 < n_class_1){
      ds <- as.integer(n_class_1/n_class_0)
      data_smote <- SMOTE(df_train_base, target = df_train_base$class,
                          K = 5, dup_size = ds)
      data_train_syn <- data_smote$syn_data[sample(nrow(data_smote$syn_data),
                                    n_class_1 - n_class_0), ]
   }else{
      ds <- as.integer(n_class_0/n_class_1)
      data_smote <- SMOTE(df_train_base, target = df_train_base$class,
                          K = 5, dup_size = ds)
      data_train_syn <- data_smote$syn_data[sample(nrow(data_smote$syn_data),
                                                   n_class_0 - n_class_1), ]
   }
   new_data_train <- rbind(data_train_syn[unique(colnames(data_train_syn))], df_train_base)
#   new_data_train <- rbind(select(data_train_syn,-class), df_train_base)
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
   df_test_output %>% select(-id)->df_test_output
   df_train_output %>% select(-id)->df_train_output
   
   fileOuputTest <- paste(".../Higgs/DataSets/test_higgs_fold_", i, sep = "")
   fileOuputTest <- paste(fileOuputTest, ".csv", sep = "")
   fileOuputTrain <- paste(".../Higgs/DataSets/train_smote_higgs_fold_", i, sep = "")
   fileOuputTrain <- paste(fileOuputTrain, ".csv", sep = "")
   write.csv(df_test_output, file=fileOuputTest, row.names = FALSE)
   write.csv(df_train_output, file=fileOuputTrain, row.names = FALSE)
}


