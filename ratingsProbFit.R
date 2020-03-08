ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
  #dataIn:
  
  # put input validation code here
  # if fails, correct it and then print msg saying we did
  
  # split into train and test set
  # predMethod & embedMeans:
  if (predMethod == 'logit') {  # check for predMethod
    if (embedMeans) {  # check if need to use embedMeans
      
    }
  } 
  
  if (predMethod == 'NMF') {  # check for predMethod
    
  }
  
  if (predMethod == 'kNN') {  # check for predMethod
    if (embedMeans) {  # check if need to use embedMeans
      
    }
  }
  
  if (predMethod == 'CART') {  # check for predMethod
    if (embedMeans) {  # check if need to use embedMeans
      
    }
  }
  
  
  # specialArgs (how?)
}





predict <- function(probsFitOut,newXs) {
  # put input validation
}


nmfFunc <- function(ratingsIn, testSet, rnk) {
  require(recosystem)
  r <- Reco()
  dataset <- data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
  testset <- data_memory(testSet[,1],testSet[,2],testSet[,3],index1=TRUE)
  r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
  result <- r$output(out_memory(),out_memory())
  preds <- r$predict(testset, out_memory())
}
