####################################
#
# Stepwise results from heart_stepwise_folds.R
#
####################################
#           Nvar      AUC
# Fold 1     11    0.9322199
# Fold 2     10    0.93882
# Fold 3      9    0.931933
# Fold 4     10    0.927112
# Fold 5      7    0.9261938
####################################

enters <- c(11, 10, 9, 10, 7, 0.9322199, 0.93882, 0.931933, 0.927112, 0.9261938)
bem_result <- matrix(enters, nrow = 5, ncol = 2)

library(ggplot2)
library(dplyr)
library(cvAUC)
nvarH <- 13
kfolds <- 5

for (i in 1:kfolds) {
  aucHeartTrain<-vector()
  aucHeartTest<-vector()
  nvarHeart<-vector()
  flagAUC <- 0
  
  for (j in 1:nvarH) {                             
    a <- paste(".../Heart/MLP/Fold", i, sep = "")
    a <- paste(a, "/MLP_results_heart_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_heart <- readRDS(a)
    
    aucValueTrain<-AUC(results_heart$predic_train[, 2], results_heart$target_train[, "1"])
    aucValueTest<-AUC(results_heart$predic_test[, 2], results_heart$target_test[, "1"])
    
#    print(aucValueTrain)
#    print(aucValueTest)
    aucHeartTrain<-c(aucHeartTrain, aucValueTrain)
    aucHeartTest<-c(aucHeartTest, aucValueTest)
    nvarHeart<-c(nvarHeart, j)
  
  }
  
  aucHeartTrain<-as.numeric(format(round(aucHeartTrain, 3), nsmall = 3))
  aucHeartTest<-as.numeric(format(round(aucHeartTest, 3), nsmall = 3))
  
  auc_result_heart <- data.frame(nvar = nvarHeart, aucTrain = aucHeartTrain, aucTest = aucHeartTest)
#  View(auc_result_heart)

  for (k in 1:nvarH){
    if (.98*auc_result_heart[["aucTrain"]][bem_result[i, 1]] < auc_result_heart[["aucTrain"]][k]){
      flagAUC <- flagAUC + 1
      if (flagAUC == 1){
        varMet <- k
        print(varMet) 
        print(auc_result_heart[["aucTrain"]][k])
        print(auc_result_heart[["aucTest"]][k])
        print("AUC reference (model):")
        print(auc_result_heart[["aucTrain"]][bem_result[i, 1]])
      }
      
    }
  }
  
  
  gTrain <- ggplot(data = auc_result_heart, 
                   aes(x = nvarHeart, y = aucHeartTrain, color = "black")) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 14) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 5) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_heart[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("text", label = "Number of variables \n from BEM", x = 5, y = 0.4, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x =5, y = 0.29, xend = 8.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("text", label = "Number of variables \n from our method", x = 2, y = 0.4, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 2, y = 0.29, xend = 7.5, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("text", label = "Threshold using BEM \n and IV ranking", x = 7, y = 0.6, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 7, y = 0.68, xend = 8.5, yend = 0.81, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (MLP)         Beyond BEM           
#           Nvar      AUC                 AUC          Nvar              AUC
#                                                                Train     Test
# Fold 1     11    0.9322199             0.964           5       0.951     0.908  
# Fold 2     10    0.93882               0.966           4       0.968     0.703   
# Fold 3      9    0.931933              0.969           4       0.959     0.879    
# Fold 4     10    0.927112              0.96            4       0.955     0.829   
# Fold 5      7    0.9261938             0.962           5       0.96      0.873  
####################################
# Average    9.4   0.9312557             0.9642         4.4      0.9586    0.8384      
####################################
mean(c(11,10,9,10,7))
mean(c(0.9322199,0.93882,0.931933,0.927112,0.9261938))
mean(c(0.964,0.966,0.969,0.96,0.962))
mean(c(5,4,4,4,5))
mean(c(0.951,0.968,0.959,0.955,0.96))
mean(c(0.908,0.703,0.879,0.829,0.873))







