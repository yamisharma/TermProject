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
}

createMatrixList <- function(dataIn) {
  f = function (rating) {
    createMatrix(rating, dataIn)
  }
  lapply(1:5, f)
}

nmfFunc <- function(ratingsIn, testSet, rnk) {
  require(recosystem)
  r <- Reco()
  
  matrixlist <- createMatrixList(ratingsIn)
  matrix <- matrixlist[[1]]
  dataset <- data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
  testset <- data_memory(testSet[,1],testSet[,2],testSet[,3],index1=TRUE)
  
  r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
  result <- r$output(out_memory(),out_memory())

  preds <- r$predict(testset, out_memory())
}


nmfFunc(trainIE, tstSong, 6)
