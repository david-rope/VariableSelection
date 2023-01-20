####################################
#
# Stepwise results from pulsar_stepwise_folds.R
#
####################################
#           Nvar      AUC
# Fold 1     8       0.9787127
# Fold 2     8       0.9796145
# Fold 3     7       0.979932
# Fold 4     7       0.9800304
# Fold 5     7       0.9791782
####################################

enters <- c(8, 8, 7, 7, 7, 0.9787127, 0.9796145, 0.979932, 0.9800304, 0.9791782)
bem_result <- matrix(enters, nrow = 5, ncol = 2)
bem_result

library(ggplot2)
library(dplyr)
library(cvAUC)
nvarH <- 8
kfolds <- 5

for (i in 1:kfolds) {
  aucPulsarTrain<-vector()
  aucPulsarTest<-vector()
  nvarPulsar<-vector()
  flagAUC <- 0
  
  for (j in 1:nvarH) {                             
    a <- paste(".../KFoldsCV/Pulsar/SVM/Fold", i, sep = "")
    a <- paste(a, "/SVM_results_pulsar_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_pulsar <- readRDS(a)
    
    aucValueTrain<-AUC(results_pulsar$predic_train[, "1"], results_pulsar$target_train)
    aucValueTest<-AUC(results_pulsar$predic_test[, "1"], results_pulsar$target_test)
    
#    print(aucValueTrain)
#    print(aucValueTest)
    aucPulsarTrain<-c(aucPulsarTrain, aucValueTrain)
    aucPulsarTest<-c(aucPulsarTest, aucValueTest)
    nvarPulsar<-c(nvarPulsar, j)
  
  }
  
  aucPulsarTrain<-as.numeric(format(round(aucPulsarTrain, 3), nsmall = 3))
  aucPulsarTest<-as.numeric(format(round(aucPulsarTest, 3), nsmall = 3))
  
  auc_result_pulsar <- data.frame(nvar = nvarPulsar, aucTrain = aucPulsarTrain, aucTest = aucPulsarTest)
#  View(auc_result_pulsar)

  for (k in 1:nvarH){
    if (.98*auc_result_pulsar[["aucTrain"]][bem_result[i, 1]] < auc_result_pulsar[["aucTrain"]][k]){
      flagAUC <- flagAUC + 1
      if (flagAUC == 1){
        varMet <- k
        print(varMet) 
        print(auc_result_pulsar[["aucTrain"]][k])
        print(auc_result_pulsar[["aucTest"]][k])
        print("AUC reference (model):")
        print(auc_result_pulsar[["aucTrain"]][bem_result[i, 1]])
      }
      
    }
  }
  
  
  gTrain <- ggplot(data = auc_result_pulsar, 
                   aes(x = nvarPulsar, y = aucPulsarTrain, color = "black")) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 9) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 5) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_pulsar[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("text", label = "Number of variables \n from BEM", x = 7, y = 0.4, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x = 7, y = 0.29, xend = 8.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("text", label = "Number of variables \n from our method", x = 4, y = 0.4, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 4, y = 0.29, xend = 7.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("text", label = "Threshold using BEM \n and IV ranking", x = 7, y = 0.6, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 7, y = 0.68, xend = 8.5, yend = 0.81, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (SVM)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar             AUC
#                                                               Train     Test
# Fold 1     8       0.9787127           0.982          1       0.968     0.975 
# Fold 2     8       0.9796145           0.984          1       0.97      0.968 
# Fold 3     7       0.979932            0.981          1       0.971     0.966  
# Fold 4     7       0.9800304           0.98           1       0.967     0.975  
# Fold 5     7       0.9791782           0.979          1       0.969     0.976  
####################################
# Average    7.4     0.9794936           0.9812         1       0.969     0.972 
####################################
mean(c(8,8,7,7,7))
mean(c(0.9787127,0.9796145,0.979932,0.9800304,0.9791782))
mean(c(0.982,0.984,0.981,0.98,0.979))
mean(c(0.968,0.97,0.971,0.967,0.969))
mean(c(0.975,0.968,0.966,0.975,0.976))

