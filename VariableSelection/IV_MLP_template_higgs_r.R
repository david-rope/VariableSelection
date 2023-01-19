####################################
##
## Ranking using Information Value
## ROC is not possible to draw due to 
## the test sample do not have target class
## We only try to see the training behavior
##
####################################
library(dplyr)

####################################
##
##  Load data
##  
####################################

file_train  <- ".../Higgs/DataSets/train_smote_higgs_fold_5.csv"
file_test <- ".../Higgs/DataSets/test_higgs_fold_5.csv"
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
####################

data_train$class <- as.factor(data_train$class)
str(data_train,list.len=ncol(data_train))

data_test$class <- as.factor(data_test$class)
str(data_test,list.len=ncol(data_test))

####################################
##
##  Training models increasing variables
##
####################################
library(Rcpp)
library(RSNNS)

x_train <- matrix(c(
  ), ncol = NVAR)
y_train <- decodeClassLabels(data_train$class)
x_test <- matrix(c(
  ), ncol = NVAR)
y_test <- decodeClassLabels(data_test$class)

all_data <- list(inputsTrain = x_train, targetsTrain = y_train,
                       inputsTest = x_test, targetsTest = y_test)
all_data <- normTrainingAndTestSet(all_data)

getNormParameters(all_data$inputsTrain)
getNormParameters(all_data$inputsTest)

nrow(x_train)
nrow(y_train)
nrow(x_test)
nrow(y_test)

####################################

print("Ppl")
ppl <- 2*NVAR
print(ppl)
nw <- rep(ppl, 2)

snns.model <- mlp(all_data$inputsTrain, all_data$targetsTrain, size=nw,
                  learnFuncParams=c(0.1), maxit=1000,
                  inputsTest=all_data$inputsTest,
                  targetsTest=all_data$targetsTest)

predic.train <- predict(snns.model, newdata=all_data$inputsTrain)
predic.test <- predict(snns.model, newdata=all_data$inputsTest)

results <- list(target_train = y_train, predic_train = predic.train,
                     target_test = y_test, predic_test = predic.test)

saveRDS(results,
        ".../Higgs/MLP/Fold5/MLP_results_higgs_iv_NVAR.rds")
saveRDS(snns.model, ".../Higgs/MLP/Fold5/MLP_model_higgs_iv_NVAR.rds")
