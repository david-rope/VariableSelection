####################################
#
# Stepwise results from bank_stepwise_folds.R
#
####################################
#
#           Nvar      AUC
# Fold 1     14     0.9419439
# Fold 2     15     0.9413761  
# Fold 3     14     0.940609      
# Fold 4     15     0.9411963                  
# Fold 5     14     0.9396963   
####################################
enters <- c(14, 15, 14, 15, 14, 0.9419439, 0.9413761, 0.940609, 0.9411963, 0.9396963)
bem_result <- matrix(enters, nrow = 5, ncol = 2)
bem_result

library(ggplot2)
library(dplyr)
library(cvAUC)
nvarH <- 15
kfolds <- 5

for (i in 1:kfolds) {
  aucBankTrain<-vector()
  aucBankTest<-vector()
  nvarBank<-vector()
  flagAUC <- 0
  
  for (j in 1:nvarH) {                             
    a <- paste(".../KFoldsCV/Bank/DT/Fold", i, sep = "")
    a <- paste(a, "/DT_results_bank_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_bank <- readRDS(a)
    
    aucValueTrain<-AUC(results_bank$predic_train[, 2], results_bank$target_train)
    aucValueTest<-AUC(results_bank$predic_test[, 2], results_bank$target_test)
    
#    print(aucValueTrain)
#    print(aucValueTest)
    aucBankTrain<-c(aucBankTrain, aucValueTrain)
    aucBankTest<-c(aucBankTest, aucValueTest)
    nvarBank<-c(nvarBank, j)
  
  }
  
  aucBankTrain<-as.numeric(format(round(aucBankTrain, 3), nsmall = 3))
  aucBankTest<-as.numeric(format(round(aucBankTest, 3), nsmall = 3))
  
  auc_result_bank <- data.frame(nvar = nvarBank, aucTrain = aucBankTrain, aucTest = aucBankTest)
#  View(auc_result_bank)

  for (k in 1:nvarH){
    if (.98*auc_result_bank[["aucTrain"]][bem_result[i, 1]] < auc_result_bank[["aucTrain"]][k]){
      flagAUC <- flagAUC + 1
      if (flagAUC == 1){
        varMet <- k
        print(varMet) 
        print(auc_result_bank[["aucTrain"]][k])
        print(auc_result_bank[["aucTest"]][k])
        print("AUC reference (model):")
        print(auc_result_bank[["aucTrain"]][bem_result[i, 1]])
      }
      
    }
  }
  
  
  gTrain <- ggplot(data = auc_result_bank, 
                   aes(x = nvarBank, y = aucBankTrain, color = "black")) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 16) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 10) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_bank[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("text", label = "Number of variables \n from BEM", x = 10, y = 0.4, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x = 9, y = 0.29, xend = 8.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("text", label = "Number of variables \n from our method", x = 2, y = 0.4, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 2, y = 0.29, xend = 7.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("text", label = "Threshold using BEM \n and IV ranking", x = 10, y = 0.6, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 10, y = 0.68, xend = 8.5, yend = 0.81, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (DT)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar      AUC
#                                                               Train      Test
# Fold 1     14      0.9419439           0.981           7      0.963      0.906
# Fold 2     15      0.9419439           0.979           7      0.961      0.89
# Fold 3     14      0.940609            0.983           7      0.965      0.901
# Fold 4     15      0.9411963           0.985           8      0.97       0.896 
# Fold 5     14      0.9396963           0.981           7      0.964      0.893 
####################################
#  Average 14.4      0.9410779           0.9818         7.2     0.9646     0.8972               
########################

mean(c(14,15,14,15,14))
mean(c(0.9419439,0.9419439,0.940609,0.9411963,0.9396963))
mean(c(0.981,0.979,0.983,0.985,0.981))
mean(c(7,7,7,8,7))
mean(c(0.963,0.961,0.965,0.97,0.964))
mean(c(0.906,0.89,0.901,0.896,0.893))









