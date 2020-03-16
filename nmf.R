library(lme4)
library(partykit)
data(InstEval)
ie <- InstEval
ie <- ie[,c(1:2,7)]
names(ie) <- c("userId", "itemId", "rating")

song <- read.csv(file="songsDataset.csv")
names(song) <- c("userId", "itemId", "rating")


testSetIndices <- sample(1: nrow(ie), 3670)
trainIE <- ie[-testSetIndices, ]
tstIE <- ie[testSetIndices, ]

testSetIndices <- sample(1: nrow(song), 100000)
trainSong <- song[-testSetIndices, ]
tstSong <- song[testSetIndices, ]


createMatrix <- function(rating, dataIn) {
  f = function (x, output) {
    if (x[3] == rating) {
      x[3] = 1
    } else {
      x[3] = 0
    }
  }
  dataIn$rating <- apply(dataIn, 1, f)
  return(dataIn)
}

createMatrixList <- function(dataIn) {
  f = function (rating) {
    createMatrix(rating, dataIn)
  }
  list <- lapply(1:5, f)
}

fitModels <- function(ratingsIn, testSet, rnk) {
  f = function(i) {
    require(recosystem)
    r <- Reco()
    
    trainlist <- createMatrixList(ratingsIn)
    train <- trainlist[[i]]
    testlist <- createMatrixList(testSet)
    test <- testlist[[i]]
    dataset <- data_memory(train[,1],train[,2],train[,3],index1=TRUE)
    testset <- data_memory(test[,1],test[,2],test[,3],index1=TRUE)
    
    r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
    result <- r$output(out_memory(),out_memory())
    
    preds <- r$predict(testset, out_memory())
  }
  
  predlist <- lapply(1:5, f)
}


fitModels(trainIE, tstSong, 6)
