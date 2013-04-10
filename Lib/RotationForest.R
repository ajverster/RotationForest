

RotationForest <- function(Df.X,Df.Y,kuse,juse,verbose = F,...) {
  # Trains a rotation forest
  #
  # Args:
  # Df.X: a data frame of X dependant vectors
  # Df.Y: a data fame of Y response values
  # kuse: The number of predictor variables to use in each rotatation
  # juse: The number of trees to train in the ensemble
  # verbose: A logical for if you want to print the progress
  # ...: Objects to pass onto the rpart function
  #
  # Returns:
  # An object of class RotationForest

  RotForest <- list()
  class(RotForest) <- "RotationForest"
  
  fits <- list()
  rots <- list()
  for (i in 1:juse) {
    model.current <- BuildOneModel(Df.X,Df.Y,kuse,...)
    fits[[i]] <- model.current[[1]]
    rots[[i]] <- model.current[[2]]
    if (verbose == T) {
      print(sprintf("Currently completed %i out of %i models",i,j.use))
    }
  }
  RotForest$models <- fits
  RotForest$rotations <- rots
  return(RotForest)
}

predict.RotationForest <- function(RotForest,Df.X, prob = F) {
  # predict() for an object of class RotationForest
  #
  # Args:
  # RotForest: An object of class RotationForest
  # Df.X: a data frame of X predictor values
  # prob: A logical indicating if you want the probabilities of being in each class, rather than the default predictions
  #
  # Returns:
  # A vector or predictions or a table of the probabilities of the different classes

  #Creates the predictions and puts them into a list
  prediction.probabilities <- list()
  for (i in 1:length(RotForest[[1]])) {
    model.current <- RotForest[[1]][[i]]
    data.current <- as.matrix(Df.X) %*% RotForest[[2]][[i]]
    data.current <- as.data.frame(data.current)
    colnames(data.current) <- paste0("X",1:ncol(data.current))
    prediction.probabilities[[i]] <- predict(model.current,data.current)
  }
  
  #Calculates the probability of each class by averaging across the different trees
  results <- matrix(ncol = ncol(prediction.probabilities[[1]]),nrow = nrow(Df.X))
  colnames(results) <- colnames(prediction.probabilities[[1]])
  for (i in 1:nrow(Df.X)) {
    results[i,] <- apply(do.call(rbind,lapply(prediction.probabilities,function(x) x[i,])),2,mean)
  }
  if (prob == TRUE)
    return(results)
  else
    return(apply(results,1,function(x) names(which(x == max(x)))))
}


BuildOneModel <- function(Df.X, Df.Y,k,rows.use.frac = 0.75,...) {
  # Builds a single decision tree using the methodoloy from Rodriguez et al. 2006
  #
  # Args:
  # Df.X: a data frame of X dependant vectors
  # Df.Y: a data fame of Y response values
  # k: The number of predictor variables to use in each rotatation
  # rows.use.frac: The fraction of data points to use in each model of the ensemble
  # ...: Objects to pass onto the rpart function
  #
  # Returns:
  # A list containing the rpart object and the rotation matrix R


  require('rpart')
  
  M <- ceiling(ncol(Df.X) / k)
  R <- matrix(nrow = ncol(Df.X),ncol = ncol(Df.X), data = 0)
  R.order <- R

  Order <- data.frame(1:ncol(Df.X),sample(sort(rep(1:k, times = M))[1:ncol(Df.X)], size = ncol(Df.X), replace = F))
  colnames(Order) <- c("V1","V2")
  
  for (i in 1:k) {
    
    rows.use <- sample(1:nrow(Df.X),size = round(rows.use.frac * nrow(Df.X)),replace = F)
    cols.use <- subset(Order,V2 == i)$V1

    #For keeping track of the position inside the rotation matrix R
    start <- (i - 1) * M + 1
    if (i != k) {
      end <- i * M
    }
    else {
      end <- ncol(Df.X)
    }
    
    Df.X.sub <- Df.X[rows.use,cols.use]
    Df.X.sub.rotation <- prcomp(Df.X.sub)$rotation
    R[start:end,start:end] <- Df.X.sub.rotation
    #Now change the position of the columns to match that of Df.X
    R.order[start:end,cols.use] <- R[start:end,start:end]
  }

  #rotate data onto the matrix
  Df.X.rotate <- as.matrix(Df.X) %*% R.order
  Df.rotate.full <- data.frame(Df.Y,Df.X.rotate)
  colnames(Df.rotate.full)[1] <- "class"
  
  fit <- rpart(class ~ ., data = Df.rotate.full,...)
  
  return.list <- list()
  return.list[[1]] <- fit
  return.list[[2]] <- R.order
  
  return(return.list)
}
