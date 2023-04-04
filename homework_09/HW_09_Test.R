library(ISLR2)
data(Weekly)
View(Weekly)

# create a vector with T or F values at the length of dataset
train <- (Year < 2009)
# use this vector to subset the train data
df_train <- Weekly[train,]
df_test <- Weekly[!train,]
# re-run the logistic regression model using only one predictor
model_logi2 <- glm(Direction~ Lag2, data = df_train, family = binomial)
# get the predicted value on the test dataset.
model_logi_est2 <- predict(model_logi2, newdata=df_test, type = "response")
# change the lable
pred_direct2 <- ifelse(model_logi_est2 >0.5, "Up", "Down")
# make a confusion matrix to compare the results
table(pred_direct2, df_test$Direction)
# we can also to get the match rate by 
accuracy <- length(pred_direct2[which(pred_direct2 == df_test$Direction)])/ nrow(df_test)
accuracy

library(MASS)
# run the LDA model
lda_fit <- lda(Direction~ Lag2, data= df_train)
# using the esitimated result to run on the test dataset
# get the predicted value on the test dataset.
lda_pred<- predict(lda_fit, newdata=df_test)
# change the lable
lda_class <- lda_pred$class
table(lda_class, df_test$Direction)
mean(lda_class == df_test$Direction)