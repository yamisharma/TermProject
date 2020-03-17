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
      cartFunc (dataIn, testSet)
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

  #View(ratingsIn)
  
  userMeans <- tapply(ratingsIn$rating, ratingsIn$userId, mean)
  itemMeans <- tapply(ratingsIn$rating, ratingsIn$itemId, mean)
  ratingsInEmb <- ratingsIn

  ratingsInEmb$userId <- userMeans[ratingsIn$userId]
  ratingsInEmb$itemId <- itemMeans[ratingsIn$itemId]

  ratingsInEmb$userId <- as.vector(ratingsInEmb$userId)
  ratingsInEmb$itemId <- as.vector(ratingsInEmb$itemId)

  ctout <- ctree(rating ~ ., data = ratingsInEmb)
  print(node_party(ctout))
}

predict <- function(probsFitOut,newXs) {
  # put input validation
}


library(lme4)
library(partykit)
data(InstEval)
ie <- InstEval
ie <- ie[,c(1:2,7)]
names(ie) <- c("userId", "itemId", "rating")

View(ie)

song <- read.csv(file="songsDataset.csv")
names(song) <- c("userId", "itemId", "rating")

testSetIndices <- sample(1: nrow(ie), 3670)
trainIE <- ie[-testSetIndices, ]
tstIE <- ie[testSetIndices, ]


testSetIndices <- sample(1: nrow(song), 100000)
trainSong <- song[-testSetIndices, ]
tstSong <- song[testSetIndices, ]

ratingProbsFit(trainIE, 5, 'CART', TRUE, 4)
