# Variable Selection 

We study the variable selection problem focus on binary classification problems.
Here, we present a simple variable selection method based on the Backward Elimination Method (BEM) and a ranking of variables using the Information Value (IV).
The next idea is divided in different stages because we used four classifiers, Logistic Regression (LR), Decision Trees (DT), Support Vector Machine (SVM) and Multiple Layer Perceptron (MLP).

The first one, it is preprocessing the data, deal with the unbalanced class problem and use K-fold cross validation.

- KFold_Scripts

The second one, it is appying the BEM and the IV ranking.

- BEM-IV

Fanally, build the required classifiers using the IV ranking and the threshold gotten from the BEM.

Here, it is possible to use a single file to build all necessary classifiers (not parallel), however, we use different files to build them.
This is because SVM and MLP can take a little time.


Note: we used two different machine for this study. Then, it is possible you need to modify some scripts.
