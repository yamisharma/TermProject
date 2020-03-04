# input the data
song_data <- read.csv("~/Documents/ecs189g/termProject/songsDataset.csv")
names(song_data) <- c("user","song","rating")

# preprocess data
##song_data$rating <- as.factor(song_data$rating)
train <- song_data[1:990000,]
test <- song_data[1990000:2000000,]

# make the model with glm
model <- glm(rating ~ .,data = train,family=gaussian())

# predict
pred <- predict(model, test)

# evaluate
print(mean(abs(pred - test$rating)))
