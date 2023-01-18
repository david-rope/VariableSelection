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

file_train  <- "/home/david/R/PaperR/TestData/Bank/new_train.csv"
file_test <- "/home/david/R/PaperR/TestData/Bank/new_test.csv"
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
#######################
##
##  The test sample is incomplete
##
#######################

ncol(data_train)
ncol(data_test)
str(data_train)
##################
## Preparing for smote method
##################
xtabs( ~ y + job, data = data_train )
xtabs( ~ y + marital, data = data_train )
xtabs( ~ y + education, data = data_train )
xtabs( ~ y + default, data = data_train )
xtabs( ~ y + housing, data = data_train )
xtabs( ~ y + loan, data = data_train )
xtabs( ~ y + contact, data = data_train )
xtabs( ~ y + month, data = data_train )
xtabs( ~ y + day_of_week, data = data_train )
xtabs( ~ y + previous, data = data_train )
xtabs( ~ y + poutcome, data = data_train )

View(data_train)

delrow <- which(grepl("yes", data_train$default))
data_train <- data_train[-delrow, ]
delrow
which(grepl("yes", data_train$default))
#########
## Splitting into the two classes
## we split 20% for train and 
## 80% test, also for k-folds cross validation
#########
nobank <- data_train[data_train$y == "no", ]
bank <- data_train[data_train$y == "yes", ]
nrow(data_train[data_train$y == "no", ])
nrow(data_train[data_train$y == "yes", ])

#####################################
##
##  Here, we do the k-folds data sets
##  We do not use the SMOTE technique for 
##  the test data set, we merge the two classes
##
#####################################
set.seed(10)
kfolds <- 5
nobank_folds <- split(nobank, sample(rep(1:kfolds)))
bank_folds <- split(bank, sample(rep(1:kfolds)))

nf <- c(1:kfolds)
for (i in 1:kfolds) {
   class_train_0 <- paste("df_nobank_train_fold_", i, sep = "")
   class_train_1 <- paste("df_bank_train_fold_", i, sep = "")
   class_test_0 <- paste("df_nobank_test_fold_", i, sep = "")
   class_test_1 <- paste("df_bank_test_fold_", i, sep = "")
   # test sample -> 20%
   new_df_test_0 <- assign(class_test_0, nobank_folds[[i]])
   new_df_test_1 <- assign(class_test_1, bank_folds[[i]])
   new_df_test <- rbind(new_df_test_0, new_df_test_1)
   df_test <- paste("df_test_fold_", i, sep = "")
   assign(df_test, new_df_test)
   # train sample -> 80%
   new_df_train_0 <- data.frame()
   new_df_train_1 <- data.frame()
   for (j in nf[-i]){
     new_df_train_0 <- rbind(new_df_train_0, nobank_folds[[j]]) 
     new_df_train_1 <- rbind(new_df_train_1, bank_folds[[j]]) 
   }
   new_df_train_0 <- assign(class_train_0, new_df_train_0) 
   new_df_train_1 <-assign(class_train_1, new_df_train_1) 
   new_df_train <- rbind(new_df_train_0, new_df_train_1)
   df_train <- paste("df_train_fold_", i, sep = "")
   assign(df_train, new_df_train)
}

nrow(new_df_train_0[new_df_test_0$y == "no", ])
nrow(new_df_train_1[new_df_test_1$y == "yes", ])
nrow(new_df_test_0[new_df_test_0$y == "no", ])
nrow(new_df_test_1[new_df_test_1$y == "yes", ])


nlevels(df_train_fold_1$default)
nlevels(df_test_fold_1$default)
nlevels(df_train_fold_2$default)
nlevels(df_test_fold_2$default)
nlevels(df_train_fold_3$default)
nlevels(df_test_fold_3$default)
nlevels(df_train_fold_4$default)
nlevels(df_test_fold_4$default)
nlevels(df_train_fold_5$default)
nlevels(df_test_fold_5$default)


#####################################
##
##  Here, we apply the SMOTE method
##  for the train data set  
##
#####################################
library(RSBID)

for (i in 1:kfolds) {
   train_name <- paste("df_train_fold_", i, sep = "")
   train_name_base <- as.name(train_name)
   df_train_base <- data.frame(eval(train_name_base))
   data_smote <- data.frame()
   data_smote <- SMOTE_NC(df_train_base, "y", perc_maj = 100, k = 5)
   smote_name <- paste("df_train_smote_fold_", i, sep = "")
   assign(smote_name, data_smote)
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
   str(df_test_output)
   fileOuputTest <- paste("/home/david/R/PaperR/TestData/KFoldsCVSMOTE/Bank/DataSets/test_bank_fold_", i, sep = "")
   fileOuputTest <- paste(fileOuputTest, ".csv", sep = "")
   fileOuputTrain <- paste("/home/david/R/PaperR/TestData/KFoldsCVSMOTE/Bank/DataSets/train_smote_bank_fold_", i, sep = "")
   fileOuputTrain <- paste(fileOuputTrain, ".csv", sep = "")
   write.csv(df_test_output, file=fileOuputTest, row.names = FALSE)
   write.csv(df_train_output, file=fileOuputTrain, row.names = FALSE)
}
