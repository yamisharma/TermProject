library(lme4)
library(partykit)
data(InstEval)
ie <- InstEval
ie <- ie[,c(1:2,7)]
names(ie) <- c("userId", "itemId", "rating")

song <- read.csv(file="songsDataset.csv")
names(song) <- c("userId", "itemId", "rating")

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



nmfFunc <- function(ratingsIn, testSet, rnk) {
  require(recosystem)
  r <- Reco()
  dataset <- data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
  testset <- data_memory(testSet[,1],testSet[,2],testSet[,3],index1=TRUE)
  r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
  result <- r$output(out_memory(),out_memory())
  
  preds <- r$predict(testset, out_memory())
  
}


cartFunc <- function(ratingsIn, testSet) {
  #ctout <- ctree(ratingsIn ~., data=mlb)
  #pred <- predict(ctout, dataToPredict, type="prob")
}

predict <- function(probsFitOut,newXs) {
  # put input validation
}
