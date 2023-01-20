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
    a <- paste(".../KFoldsCV/Fraud/DT/Fold", i, sep = "")
    a <- paste(a, "/DT_results_fraud_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_fraud <- readRDS(a)
    
    aucValueTrain<-AUC(results_fraud$predic_train[, 2], results_fraud$target_train)
    aucValueTest<-AUC(results_fraud$predic_test[, 2], results_fraud$target_test)
    
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
#                 BEM              Reference (DT)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar             AUC
#                                                                Train     Test 
# Fold 1     29     0.9970249            0.999          3        0.995     0.965 
# Fold 2     30     0.998279               1            2        0.982     0.924   
# Fold 3     30     0.9975251              1            3        0.993     0.924   
# Fold 4     28     0.9975678              1            3        0.996     0.934   
# Fold 5     29     0.9981895            0.999          3        0.991     0.871     
####################################
# Average   29.2    0.9977173            0.9996         2.8      0.9914    0.9236                                           
####################################
mean(c(29,30,30,28,29))
mean(c(0.9970249,0.998279,0.9975251,0.9975678,0.9981895))
mean(c(0.999,1,1,1,0.999))
mean(c(3,2,3,3,3))
mean(c(0.995,0.982,0.993,0.996,0.991))
mean(c(0.965,0.924,0.924,0.934,0.871))


