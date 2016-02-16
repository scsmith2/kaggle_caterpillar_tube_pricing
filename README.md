### Caterpillar Tube Pricing on Kaggle (Aug 2015)

### Objective
Predict quoted price for hydraulic tube assemblies based on product and supply chain features.

### Results
The script in this repository resulted in an RMSLE of 0.2181, resulting in an overall position of **95th on the [leaderboard](https://www.kaggle.com/c/caterpillar-tube-pricing/leaderboard/private)** in a field of 1323 individuals and teams.

Submissions were evaluated on the Root Mean Squared Logarithmic Error, calculated as:

![rmsle](/imgs/rmsle_eqn.png)

Where:
* 'n' is the number of price quotes in the test set
* 'p_i' is the predicted price
* 'a_i' is the actual price
* 'log(x)' is the natural logarithm

### Approach
Python and R were used to develop the prediction algorithm.  Three types of models were combined with varying inputs and tuning parameters.  These were [random forests](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf) in R, [support vector machines](https://cran.r-project.org/web/packages/e1071/e1071.pdf) in R, and [boosted trees](https://xgboost.readthedocs.org/en/latest/) using the xgboost package in both R and Python.  [Boosting and stacking](http://stats.stackexchange.com/questions/18891/bagging-boosting-and-stacking-in-machine-learning) was applied to reduce the propensity to overfit to the training set and achieve stronger predictions.

### Model Architecture
The model architecture is shown graphically in below.
<img src="/imgs/cat_model_architecture.png" width="700"/>

**Script Description and Run Order**

Executing the scripts as they are ordered satisfies input dependencies.

* Training / Competition Data: 20 csv files forming a relational dataset (e.g. part lists, BoMs, material specifications)
* cat_preproc.R: Cleaning and processing of the original data
* cat_xgb_1.R:  3 xgboost models with averaged output
* cat_xgb_2.R:  3 xgboost models with averaged output
* cat_xgb_3.R:  3 xgboost models with averaged output
* cat_kaggle_script.py: 4 xgboost models with combined output
* cat_xgb.R: xgboost with 10-fold cross validation
* cat_svm.R: support vector machine with 10-fold cross validation
* cat_rf.R: random forest with 10-fold cross validation
* cat_combine_1.R: 3 xgboost models, 1 random forest model, 1 support vector machine --- combined by stacking with the training data in a random forest.  The resulting random forest output was then averaged with 3 xgboost models
* cat_combine_2.R: averaging of the output from cat_kaggle_script.py and cat_combine_1.R
* cat_stack.R: 1 support vector machine, 1 random forest and 1 xgboost model --- combined by stacking with the training data in a random forest.  This resulted in the final competition submission.
