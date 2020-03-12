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
model <- knn(embedded_train, embedded_test, NULL, k = 5, prob = true)

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


