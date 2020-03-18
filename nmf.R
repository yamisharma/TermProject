library(lme4)
data(InstEval)
ie <- InstEval
ie <- ie[,c(1:2,7)]
names(ie) <- c("userId", "itemId", "rating")
ie$userId <- as.factor(ie$userId)
ie$itemId <- as.factor(ie$itemId)
ie$rating <- as.factor(ie$rating)

song <- read.csv(file="songsDataset.csv")
names(song) <- c("userId", "itemId", "rating")
song$userId <- as.factor(song$userId)
song$itemId <- as.factor(song$itemId)
song$rating <- as.factor(song$rating)

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

fitModels <- function(ratingsIn, testSet, rnk=20) {
  trainlist <- createMatrixList(ratingsIn)
  testlist <- createMatrixList(testSet)
  
  f = function(i) {
    require(recosystem)
    r <- Reco()
    train <- trainlist[[i]]
    test <- testlist[[i]]
    dataset <- data_memory(train[,1],train[,2],train[,3],index1=TRUE)
    testset <- data_memory(test[,1],test[,2],test[,3],index1=TRUE)
    
    # set.seed(123) 
    # opts_tune <- r$tune(trainset)$min
    # print(opts_tune)
    
    # tune function indicates rank 20 to be optimal
    r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
    preds <- r$predict(testset, out_memory())
  }
  
  predList <- lapply(1:5, f)
}


list <- fitModels(trainIE, tstIE, 20)
mat <- cbind(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]])

