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
    a <- paste(".../KFoldsCV/Bank/LR/Fold", i, sep = "")
    a <- paste(a, "/LR_results_bank_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_bank <- readRDS(a)
    
    aucValueTrain<-AUC(results_bank$predic_train, results_bank$target_train)
    aucValueTest<-AUC(results_bank$predic_test, results_bank$target_test)
    
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
                   aes(x = nvarBank, y = aucBankTrain, color = "black"), theme(plot.margin = unit(c(-4.0,-1.0,-4.0,-1.0), "cm"))) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 16) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 10) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_bank[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("label", label = "Number of variables \n from BEM", x = 11, y = 0.5, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x = 11, y = 0.44, xend = bem_result[i, 1], yend = 0.3, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("label", label = "Number of variables \n from our method", x = 4, y = 0.5, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 4, y = 0.44, xend = varMet, yend = 0.3, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("label", label = "Threshold using BEM \n and IV ranking", x = 11, y = 0.75, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 11, y = 0.81, xend = bem_result[i, 1], yend = auc_result_bank[["aucTrain"]][bem_result[i, 1]] , curvature = -.2,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (LR)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar      AUC
#                                                               Train     Test
# Fold 1     14      0.9419439          0.942           7       0.936     0.895 
# Fold 2     15      0.9419439          0.941           6       0.929     0.894
# Fold 3     14      0.940609           0.941           7       0.935     0.901 
# Fold 4     15      0.9411963          0.941           7       0.936     0.892 
# Fold 5     14      0.9396963          0.94            7       0.934     0.899 
####################################
#  Average 14.4      0.9410779          0.941           6.8     0.934     0.8962         
########################

mean(c(14,15,14,15,14))
mean(c(0.9419439,0.9419439,0.940609,0.9411963,0.9396963))
mean(c(0.942,0.941,0.941,0.941,0.94))
mean(c(7,6,7,7,7))
mean(c(0.936,0.929,0.935,0.936,0.934))
mean(c(0.895,0.894,0.901,0.892,0.899))
