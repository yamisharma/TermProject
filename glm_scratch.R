df <- read.csv("/Users/yaminisharma/desktop/MOOC.csv")[1:1000,]
View(df)
df$r45 <- r45
glmout <- glm(r45~registered+certified, data = df, family = binomial)
glmout

####
#Call:  glm(formula = r45 ~ registered + certified, family = binomial, 
 #          data = df)

#Coefficients:
#  (Intercept)   registered    certified  
#-2.694           NA       19.260  

#Degrees of Freedom: 701 Total (i.e. Null);  700 Residual
#(298 observations deleted due to missingness)
#Null Deviance:	    365.7 
#Residual Deviance: 328 	AIC: 332
##
predict(glmout)