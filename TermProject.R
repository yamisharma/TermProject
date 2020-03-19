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

## LOAD DATA## 

#KNN HELPER FUNCTION
predict_knn <- function(dataIn, maxRating, targetUser, targetItem, k){
  names(dataIn) <- c("UserID","ItemID","rating")
  # Only consider relevant users
  new_users <- as.numeric(as.vector(dataIn[dataIn$ItemID == targetItem,1]))
  new_users <- new_users[new_users != targetUser]
  if (k > length(new_users)) {
    tryCatch("k must be <= num of relevant users")
  }
  # Compare dist b/w users
  user_dists <- matrix(0,0,2)
  for (user_itr in new_users) {
    user_dists <- rbind(user_dists, c(user_itr,dist(x=rbind(as.vector(dataIn$ItemID[dataIn$UserID == targetUser]),as.vector(dataIn$ItemID[dataIn$UserID == user_itr])))))
  }
  # Create neighborhood (choose k closest)
  user_dists <- as.data.frame(user_dists)
  user_dists <- user_dists[order(user_dists$V2),]
  user_dists <- user_dists[1:k,]
  # Make list of all ratings from neighbors
  neigh_rats <- matrix(0,0,1)
  for (user_itr in user_dists$V1) {
    neigh_rats <- rbind(neigh_rats, dataIn$rating[dataIn$UserID == user_itr & dataIn$ItemID == targetItem])
  }
  return (list("knn", neigh_rats))
}

## RATINGSPROBFIT
ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
  require(lme4)
  require(partykit)
  require(recosystem)
  
  names(dataIn) <- c("userId", "itemId", "rating")
  
  if(predMethod == "logit") {
    # loop for maxrating which is 5 in our case
    for(i in 1: maxRating){
      rating_known <- as.integer(dataIn$rating == i)
      rating_known <- as.factor(rating_known)
      data_known <- data.frame(dataIn[1:2],rating_known)
      
      test<-sample(1:nrow(data_known),nrow(data_known))
      testSet <- data_known[test,]
      trainSet <- data_known[-test,]
      
      glmout <- glm(rating_known ~ userID + itemID, data = trainSet, family = binomial)
      output[[glm_output]] <- glmout #into an array
    }
    probsFitOut <- list(dataIn = dataIn, predMethod = "logit", maxRating = maxRating,specialArgs = output)
    class(probsFitOut)<-"recProbs"
    return(probsFitOut)
  }
  
  # for nmf, the specialArgs must contain the rank
  if (predMethod == 'NMF') {
    probsFitOut <- list(rank=specialArgs[["rank"]], dataIn=dataIn, predMethod="nmf")
    class(probsFitOut) <- "recProbs"
    return (probsFitOut)
  }
  
  # for knn, the specialArgs must contain the value of k, target user, and the target item (all ints)
  if (predMethod == "kNN") {
    probsFitOut <- predict_knn(dataIn, maxRating, specialArgs[1], specialArgs[2], specialArgs[3])
    class(probsFitOut)<-"recProbs"
    return(probsFitOut)
  }
  
  if (predMethod == 'CART') { 
    if (embedMeans) { 
      probsFitOut <- cartFunc(dataIn)
      class(probsFitOut)<-"recProbs"
      return(probsFitOut)
    }
    else {
      tryCatch("CART requires embedMeans")
    }
  }
}

## NMF HELPER FUNCTIONS
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

predictNmf <- function(probsFitOut, newXS) {
  rnk <- probsFitOut[[1]]
  train <- probsFitOut[[2]]
  newXS$rating <- NA
  tst <- newXS
  mat <- fitModels(train, tst, rnk)
  return(mat)
}

##PREDICT.RECPROBS
predict.recProbs <- function(probsFitOut,newXs) {
  if (probsFitOut[[1]] == "knn") {
    neigh_rats <- recProbsFit[[2]]
    # Take probab equal to ratio of one rating num to the others
    probs_out <- matrix(0,0,1)
    for (itr in 1:maxRating) {
      probs_out <- rbind(probs_out, (length(neigh_rats[neigh_rats == itr]) / length(neigh_rats)))
    }
    return(probs_out)
  }
  
  if(probsFitOut$predmethod == "logit"){
    for(i in 1: probsFitOut$maxRating){
      pred_output <- predict.glm(probsFitOut$,newXs,type = "response")
      final_pred <- cbind(final_pred,pred_output)
    }
    return (final_pred)
  }
  
  if (probsFitOut[["predMethod"]] == "nmf") {
    pred_out <- predictnmf(probsFitOut, newXs)
    return (pred_out)
  }
  
  if(probsFitOut[["predMethod"]] == "CART") {
    newXS$userId <- as.factor( newXS$userId)
    newXS$itemId <- as.factor( newXS$itemId)
      
    userMeans <- tapply( newXS$rating, newXS$userId, mean)
    itemMeans <- tapply( newXS$rating, newXS$itemId, mean)
    ratingsInEmb <- newXS
      
    ratingsInEmb$userId <- userMeans[ratingsIn$userId]
    ratingsInEmb$itemId <- itemMeans[ratingsIn$itemId]
      
    ratingsInEmb$userId <- as.vector(ratingsInEmb$userId)
    ratingsInEmb$itemId <- as.vector(ratingsInEmb$itemId)
    for(i in 1: probsFitOut$maxRating){
      pred_output <- predict.CART(probsFitOut$,ratingsInEmb,type = "response")
      final_pred <- cbind(final_pred,pred_output)
      return (final_pred)
    }
  }
  
}
