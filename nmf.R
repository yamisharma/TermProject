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


createMatrix <- function(rating, data) {
  f = function (x, output) {
    if (x[3] == rating) {
      x[3] = 1
    } else {
      x[3] = 0
    }
  }
  data$rating <- apply(data, 1, f)
  return(data)
}

createMatrixList <- function(data) {
  f = function (rating) {
    createMatrix(rating, data)
  }
  list <- lapply(1:5, f)
}

fitModels <- function(ratingsIn, testSet, rnk) {
  trainlist <- createMatrixList(ratingsIn)

  f = function(i) {
    require(recosystem)
    r <- Reco()
    train <- trainlist[[i]]
    dataset <- data_memory(train[,1],train[,2],train[,3],index1=TRUE)
    testset <- data_memory(testSet[,1],testSet[,2],testSet[,3],index1=TRUE)
    
    # set.seed(123) 
    # opts_tune <- r$tune(trainset)$min
    # print(opts_tune)
    
    # tune function indicates rank 20 to be optimal
    r$train(dataset, opts=list(dim=rnk, nmf=TRUE))
    preds <- r$predict(testset, out_memory())
  }
  
  list <- lapply(1:5, f)
  mat <- cbind(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]])
  name(mat) <- c("probRating1", "probRating2", "probRating3", "probRating4", "probRating5")
}

probsFitNmf <- function(dataIn, maxRating, predMethod, specialArgs) {
  probsFitOut <- list(specialArgs[["rank"]],dataIn)
  class(probsFitOut) <- "recProbs"
  return (probsFitOut)
}

predictNmf <- function(probsFitOut, newXS) {
  rnk <- probsFitOut[[1]]
  train <- probsFitOut[[2]]
  newXS$rating <- NA
  tst <- newXS
  mat <- fitModels(train, tst, rnk)
}
