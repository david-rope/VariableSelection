####################################
#
# Stepwise results from fraud_stepwise_folds.R
#
####################################
#           Nvar      AUC
# Fold 1     29     0.9970249     
# Fold 2     30     0.998279     
# Fold 3     30     0.9975251       
# Fold 4     28     0.9975678    
# Fold 5     29     0.9981895     
####################################

enters <- c(29, 30, 30, 28, 29, 0.9970249, 0.998279, 0.9975251, 0.9975678, 0.9981895)
bem_result <- matrix(enters, nrow = 5, ncol = 2)

library(ggplot2)
library(dplyr)
library(cvAUC)
nvarH <- 30
kfolds <- 5

for (i in 1:kfolds) {
  aucFraudTrain<-vector()
  aucFraudTest<-vector()
  nvarFraud<-vector()
  flagAUC <- 0
  
  for (j in 1:nvarH) {                             
    a <- paste(".../Fraud/MLP/Fold", i, sep = "")
    a <- paste(a, "/MLP_results_fraud_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_fraud <- readRDS(a)
    
    aucValueTrain<-AUC(results_fraud$predic_train[, 2], results_fraud$target_train[, "1"])
    aucValueTest<-AUC(results_fraud$predic_test[, 2], results_fraud$target_test[, "1"])
    
#    print(aucValueTrain)
#    print(aucValueTest)
    aucFraudTrain<-c(aucFraudTrain, aucValueTrain)
    aucFraudTest<-c(aucFraudTest, aucValueTest)
    nvarFraud<-c(nvarFraud, j)
  
  }
  
  aucFraudTrain<-as.numeric(format(round(aucFraudTrain, 3), nsmall = 3))
  aucFraudTest<-as.numeric(format(round(aucFraudTest, 3), nsmall = 3))
  
  auc_result_fraud <- data.frame(nvar = nvarFraud, aucTrain = aucFraudTrain, aucTest = aucFraudTest)
#  View(auc_result_fraud)

  for (k in 1:nvarH){
    if (.98*auc_result_fraud[["aucTrain"]][bem_result[i, 1]] < auc_result_fraud[["aucTrain"]][k]){
      flagAUC <- flagAUC + 1
      if (flagAUC == 1){
        varMet <- k
        print(varMet) 
        print(auc_result_fraud[["aucTrain"]][k])
        print(auc_result_fraud[["aucTest"]][k])
        print("AUC reference (model):")
        print(auc_result_fraud[["aucTrain"]][bem_result[i, 1]])
      }
      
    }
  }
  
  
  gTrain <- ggplot(data = auc_result_fraud, 
                   aes(x = nvarFraud, y = aucFraudTrain, color = "black")) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 31) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_fraud[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("text", label = "Number of variables \n from BEM", x = 17, y = 0.4, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x = 17, y = 0.29, xend = 20.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("text", label = "Number of variables \n from our method", x = 4, y = 0.4, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 4, y = 0.29, xend = 7.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("text", label = "Threshold using BEM \n and IV ranking", x = 25, y = 0.6, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 25, y = 0.68, xend = 21.5, yend = 0.81, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (MLP)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar            AUC
#                                                                Train     Test  
# Fold 1     29     0.9970249              1            4        0.987     0.983 
# Fold 2     30     0.998279               1            1        0.982     0.938
# Fold 3     30     0.9975251              1            4        0.984     0.955
# Fold 4     28     0.9975678              1            3        0.982     0.954 
# Fold 5     29     0.9981895              1            4        0.987     0.938   
####################################
# Average   29.2    0.9977173              1            3.8      0.9844    0.9536                                
####################################
mean(c(29,30,30,28,29))
mean(c(0.9970249,0.998279,0.9975251,0.9975678,0.9981895))
mean(c(4,1,4,3,4))
mean(c(0.987,0.982,0.984,0.982,0.987))
mean(c(0.983,0.938,0.955,0.954,0.938))



  