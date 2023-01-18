Variable Selection 

We study the variable selection problem focus on binary classification problems.
Here, we present a simple variable selection method based on the Backward Elimination Method (BEM) and a ranking of variables using the Information Value (IV).
The next idea is divided in different stages because we used four classifiers, Logistic Regression, Decision Trees, Support Vector Machine and Multiple Layer Perceptron.

The first one, it is preprocessing the data, deal with the unbalanced class problem and use K-fold cross validation.
The second one, it is appying the BEM and the IV ranking.
Fanally, build the required classifiers using the IV ranking and the threshold gotten from the BEM.

Note: we used two different machine for this study. Then, it is possible you need to modify some scripts.
