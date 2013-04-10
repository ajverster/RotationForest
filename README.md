An implementation of the "Rotation Forest" algorithm from Rodriguez et al. 2006 in R. In outline it is an ensemble of decision trees, like Random Forest, but every tree has the features randomly split into k subsets and Principal Component Analysis is applied separately to each subset. The decision tree used in the ensemble is rpart(). 

RotationForest.R contains the functions required to train and predict with a RotationForest object

Usage.R contains an example comparing the results between RandomForest and RotationForest on a classification task from the UCI Repository
