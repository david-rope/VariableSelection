####################################
#
# Stepwise results from higgs_stepwise_folds.R
#
####################################
#           Nvar                  AUC
# Fold 1     18       0.6862255     
# Fold 2     20       0.6867432   
# Fold 3     20       0.6860155   
# Fold 4     17       0.6853733    
# Fold 5     18       0.6854524   
####################################

enters <- c(18, 20, 20, 17, 18, 0.6862255, 0.6867432, 0.6860155, 0.6853733, 0.6854524)
bem_result <- matrix(enters, nrow = 5, ncol = 2)
bem_result

library(ggplot2)
library(dplyr)
library(cvAUC)
nvarH <- 28
kfolds <- 5

for (i in 1:kfolds) {
  aucHiggsTrain<-vector()
  aucHiggsTest<-vector()
  nvarHiggs<-vector()
  flagAUC <- 0
  
  for (j in 1:nvarH) {                             
    a <- paste(".../KFoldsCV/Higgs/LR/Fold", i, sep = "")
    a <- paste(a, "/LR_results_higgs_iv_", sep = "")
    a <- paste(a, j, sep="")
    a <- paste(a, ".rds", sep="")
#    print(a)
    results_higgs <- readRDS(a)
    
    aucValueTrain<-AUC(results_higgs$predic_train, results_higgs$target_train)
    aucValueTest<-AUC(results_higgs$predic_test, results_higgs$target_test)
    
#    print(aucValueTrain)
#    print(aucValueTest)
    aucHiggsTrain<-c(aucHiggsTrain, aucValueTrain)
    aucHiggsTest<-c(aucHiggsTest, aucValueTest)
    nvarHiggs<-c(nvarHiggs, j)
  
  }
  
  aucHiggsTrain<-as.numeric(format(round(aucHiggsTrain, 3), nsmall = 3))
  aucHiggsTest<-as.numeric(format(round(aucHiggsTest, 3), nsmall = 3))
  
  auc_result_higgs <- data.frame(nvar = nvarHiggs, aucTrain = aucHiggsTrain, aucTest = aucHiggsTest)
#  View(auc_result_higgs)

  for (k in 1:nvarH){
    if (.98*auc_result_higgs[["aucTrain"]][bem_result[i, 1]] < auc_result_higgs[["aucTrain"]][k]){
      flagAUC <- flagAUC + 1
      if (flagAUC == 1){
        varMet <- k
        print(varMet) 
        print(auc_result_higgs[["aucTrain"]][k])
        print(auc_result_higgs[["aucTest"]][k])
        print("AUC reference (model):")
        print(auc_result_higgs[["aucTrain"]][bem_result[i, 1]])
      }
    }
  }
  
  gTrain <- ggplot(data = auc_result_higgs, 
                   aes(x = nvarHiggs, y = aucHiggsTrain, color = "black"), theme(plot.margin = unit(c(-4.0,0.1,-4.0,0.1), "cm"))) +
    geom_point(color="black") + geom_line(color="black") + xlab("Number of variables") + ylab("AUC") + xlim(1, 29) + ylim(0, 1)
  gTrain <- gTrain +  coord_fixed(ratio = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_text(face = "bold", size = rel(1))) +
    geom_vline(xintercept = varMet, linetype="dotted", color = "magenta", size=1.0) +
    geom_vline(xintercept = bem_result[i, 1], linetype="dotted", color = "red", size=1.0) +
    geom_hline(yintercept = auc_result_higgs[["aucTrain"]][bem_result[i, 1]] , linetype="dotted", color = "blue", size=1.0) +
    annotate("label", label = "Number of variables \n from BEM", x = 13, y = 0.4, size = 3, colour = "red" ) +
    #  annotate("text", label = "from BEM", x = 17, y = 0.3, size = 3, colour = "red") +
    annotate("curve", x = 13, y = 0.325, xend = bem_result[i, 1], yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "red") +
    annotate("label", label = "Number of variables \n from our method", x = 3.5, y = 0.4, size = 3, colour = "magenta" ) +
    #  annotate("text", label = "", x = 4, y = 0.3, size = 3, colour = "magenta" ) +
    annotate("curve", x = 3.5, y = 0.325, xend = varMet, yend = 0.2, curvature = .3,
             arrow = arrow(length = unit(2, "mm")), colour = "magenta") +
    annotate("label", label = "Threshold using BEM \n and IV ranking", x = 23, y = 0.9, size = 3, colour = "blue" ) +
    #  annotate("text", label = "and IV ranking", x = 25, y = 0.75, size = 3, colour = "blue" ) 
    annotate("curve", x = 23, y = 0.825, xend = bem_result[i, 1], yend = auc_result_higgs[["aucTrain"]][bem_result[i, 1]] , curvature = -.2,
             arrow = arrow(length = unit(2, "mm")), colour = "blue") 
  print(gTrain)
  
}

####################################
#
#                 BEM              Reference (LR)         Beyond BEM           
#           Nvar         AUC              AUC          Nvar          AUC
#                       Train            Train                  Train     Test
# Fold 1     18       0.6862255          0.684          8       0.677     0.673
# Fold 2     20       0.6867432          0.686          8       0.678     0.671
# Fold 3     20       0.6860155          0.683          8       0.677     0.67
# Fold 4     17       0.6853733          0.684          8       0.675     0.681
# Fold 5     18       0.6854524          0.685          8       0.676     0.674
####################################
# Average    18.6     0.685962           0.6844         8       0.6778    0.6738
###################################
mean(c(18,20,20,17,18))
mean(c(0.6862255,0.6867432,0.6860155,0.6853733,0.6854524))
mean(c(0.684,0.686,0.683,0.684,0.685))
mean(c(0.677,0.684,0.677,0.675,0.676))
mean(c(0.673,0.671,0.67,0.681,0.674))



