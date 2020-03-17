require(rectools)

# input the data
song_data <- read.csv("~/Documents/ecs189g/termProject/songsDataset.csv")
names(song_data) <- c("UserID","ItemID","rating")

# preprocess data
song_data$rating <- as.numeric(song_data$rating)
song_data$UserID <- as.factor(song_data$UserID)
song_data$ItemID <- as.factor(song_data$ItemID)

##song_data$rating <- as.factor(song_data$rating)
## FIXME remember to make sure users are in both test and train sets!!
##train <- song_data[1:990000,]
train <- song_data[1:100,]
##test <- song_data[1990000:2000000,]
test <- song_data[101:110,]

# embedding: use mean rating by user instead of userID
embedded_train <- embedMeans(train)
embedded_test <- embedMeans(test)

# use knn
model <- knn(embedded_train, embedded_test, embedded_train, k = 5, prob = true)

# predict
pred <- predict(model, test)

# evaluate
print(mean(abs(pred - test$rating)))


# Add to the manual that the naming convention for this dataframe.
embedMeans <- function(dataIn){
  # find the different users
  users <- unique(dataIn$UserID)
  # now find the mean ratings
  meanUserRatings <- data.frame()
  for (user in users) {
    meanUserRatings <- rbind(meanUserRatings, mean(dataIn$rating[dataIn$UserID == user]))
  }
  return(meanUserRatings)
}

my_knn <- function(dataIn, targetUser, targetItem, k){
  names(dataIn) <- c("UserID","ItemID","rating")
  # Only consider relevant users
  new_users <- as.numeric(as.vector(dataIn[dataIn$ItemID == targetItem,1]))
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
  user_dists <- user_dists[2:(k-1),]
  # Make list of all ratings from neighbors
  neigh_rats <- matrix(0,0,1)
  for (user_itr in user_dists$V1) {
    neigh_rats <- rbind(neigh_rats, dataIn$rating[dataIn$UserID == user_itr & dataIn$ItemID == targetItem])
  }
  # Take probab equal to ratio of one rating num to the others
  prob1 <- length(neigh_rats[neigh_rats == 1]) / length(neigh_rats)
  prob2 <- length(neigh_rats[neigh_rats == 2]) / length(neigh_rats)
  prob3 <- length(neigh_rats[neigh_rats == 3]) / length(neigh_rats)
  prob4 <- length(neigh_rats[neigh_rats == 4]) / length(neigh_rats)
  prob5 <- length(neigh_rats[neigh_rats == 5]) / length(neigh_rats)
  
  print(prob1)
  print(prob2)
  print(prob3)
  print(prob4)
  print(prob5)
}
my_knn(song_data, 55346, 0, 5)
