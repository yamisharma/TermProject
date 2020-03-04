# read in data
data <- read.csv("~/Documents/ecs189g/termProject/harvardData.csv")[7:11]
##NOTE: the grade data is numeric. If we want to predict probability
##of certain outcome, I think we need it to be categorical, but that
##would grade a lot of categories.

# preprocess data
testIndxs <- sample(1: (.1*nrow(data)))
train <- data[- testIndxs,]
test <- data[testIndxs,]

# make the model with glm
model <- glm(grade ~ .,data = train,family=gaussian())