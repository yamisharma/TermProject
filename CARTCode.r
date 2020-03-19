cartFunc <- function(ratingsIn) {

  #View(ratingsIn)
  ratingsIn$userId <- as.factor(ratingsIn$userId)
  ratingsIn$itemId <- as.factor(ratingsIn$itemId)

  userMeans <- tapply(ratingsIn$rating, ratingsIn$userId, mean)
  itemMeans <- tapply(ratingsIn$rating, ratingsIn$itemId, mean)
  ratingsInEmb <- ratingsIn

  ratingsInEmb$userId <- userMeans[ratingsIn$userId]
  ratingsInEmb$itemId <- itemMeans[ratingsIn$itemId]

  ratingsInEmb$userId <- as.vector(ratingsInEmb$userId)
  ratingsInEmb$itemId <- as.vector(ratingsInEmb$itemId)

  ctout <- ctree(rating ~ ., data = ratingsInEmb (#maybe have the training set here @aditi) not sure. Do we do CV on the emb dataset to create a new train set??)
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
    
 ####
 elseif(probsFitOut == 'CART'){
  #replace ratings In with newXS --- newXS is basically the test set from emb in the org cart func above
   newXS$userId <- as.factor( newXS$userId)
   newXS$itemId <- as.factor( newXS$itemId)

  userMeans <- tapply( newXS$rating, newXS$userId, mean)
  itemMeans <- tapply( newXS$rating, newXS$itemId, mean)
  ratingsInEmb <- newXS

  ratingsInEmb$userId <- userMeans[ratingsIn$userId]
  ratingsInEmb$itemId <- itemMeans[ratingsIn$itemId]

  ratingsInEmb$userId <- as.vector(ratingsInEmb$userId)
  ratingsInEmb$itemId <- as.vector(ratingsInEmb$itemId)
#use in built predict function and take test set from above + the tree that CART spits out as args. Divide this pred input with test set (well not the set but maybe take the abs value or sthg) and assign that as a probability variable
#then return the latest pred
   
   }
