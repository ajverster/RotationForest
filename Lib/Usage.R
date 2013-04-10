#A demonstration of RotationForest() on "balance-scale.data"


############
#Libraries
############
source("RotationForest.R")
library('randomForest')
library('rpart')

############
#Functions
############

CV_Accuracy <- function(Df.Y,Df.X,algorithm,k.cv,...) {
  # Calculates the cross validation accuracy of a given algorithm 
  #
  # Arguments:
  # Df.X: A data frame of X dependant vectors
  # Df.Y: A data fame of Y response values
  # algorithm: The name of the algorithm you are testing
  # k.cv: Number of folds for cross validation
  # ...: Additional arguments to be passed to your algorithm
  #
  # Returns:
  # A vector of accuracies for the predictions in each fold of the cross validation
  
  Data <- data.frame(Df.Y,Df.X)
  
  FUN <- match.fun(algorithm)
  CV.groups <- sample(rep(1:k.cv,times = ceiling(nrow(Data) / k.cv)),size = nrow(Data), replace = F)
  Accuracies <- vector(mode = "numeric",length = length(k.cv))
  for (i in 1:k.cv) {
    #split into training and test sets
    Data.train <- Data[!CV.groups%in%i,]
    Data.test <- Data[CV.groups%in%i,]
    #Create the model
    fit <- FUN(Data.train[,-1],Data.train[,1],...)
    
    #Calculate the accuracy
    accuracy <- sum(Data.test[,1] == predict(fit,Data.test[,-1])) / nrow(Data.test) * 100
    Accuracies[i] <- accuracy
  }
  return(Accuracies)
}

############
#Paramters
############

features.group <- 2
n.trees <- 10
k.cv.use <- 10

############
#Input
############

Data <- read.table("../Data/balance-scale.data", sep = ",", header = F)
Data.Y <- Data[,1]
Data.X <- Data[,-1]
Data.Y <- as.factor(Data.Y)

set.seed(7)
acc.RandForest <- CV_Accuracy(Data.Y,Data.X,randomForest,k.cv = k.cv.use)
acc.RotForest <- CV_Accuracy(Data.Y,Data.X,RotationForest,k.cv = k.cv.use,kuse = features.group,juse = n.trees)

print(sprintf("Mean accuracy with RandomForest is %f, and RotationForest is %f",mean(acc.RandForest),mean(acc.RotForest)))
