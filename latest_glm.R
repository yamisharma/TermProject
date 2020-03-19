ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
colnames(dataIn) <- c("userID", "itemID", "rating")
  
##test <- as.integer(df$grade >= 0.01)
#df$test <- test
#glmout <- glm(r45~registered+certified, data = df, family = binomial)
#glmout
##predict(glmout)

--
  if(predMethod == "logit"){
  #loop for maxrating which is 5 in our case?
  for(i in 1: maxRating){
    rating_known <- as.integer(dataIn$rating == i)
    rating_known <- as.factor(rating_known)
   
    data_known <- data.frame(dataIn[1:2],rating_known)
    data_f<-sample(1:nrow(data_known),0.05*nrow(data_known))
    
    testSet<-data_known[data_f,]
    trainSet<-data_known[-data_f,]
    
    glmout <- glm(rating_known ~ userID + itemID, data = trainSet, family = binomial)
    output[[glm_output]] <- glmout
    
  }
  probsFitOut <- list(dataIn = dataIn, predMethod = "logit", maxRating = maxRating,specialArgs = output)
    
  #s3 class
  class(probsFitOut)<-"recProbs" 
}

# predict probabilities under predict.recProbs <- (probsFitOut, newXs)

if(probsFitOut/predmethod == "logit"){
  for(i in 1: probsFitOut$maxRating){
    pred_output <- predict.glm(probsFitOut$,newXs,type = "response")
    final_pred <- cbind(final_pred,pred_output)
  }
  data(final_pred)<-(1:probsFitOut$maxRating)

#run test
  
# Results of calling glmout (note: this is not from calling predict)
#Call:  glm(formula = test ~ registered + certified, family = binomial, data = df)

#Coefficients:
#  (Intercept)   registered    certified  
#-2.694           NA       19.260  

#Degrees of Freedom: 701 Total (i.e. Null);  700 Residual
#(298 observations deleted due to missingness)
#Null Deviance:	    365.7 
#Residual Deviance: 328 	AIC: 332

