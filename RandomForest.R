df <- read.csv("diabetes.csv")
View(df)
plot(df$Age,df$Outcome)
str(df)
colSums(is.na(df))
table(df$Outcome)
library(caret)
library(randomForest)
df$Outcome <- as.factor(df$Outcome)
table(df$Outcome)
plot(df$Outcome,df$Age)

# data splitting
library(caTools)
index <- sample.split(df$Outcome,SplitRatio = 0.70)
train <- subset(df,index==TRUE)
test <- subset(df,index==FALSE)
table(test$Outcome)/230
table(train$Outcome)/538

model_RF <- randomForest(Outcome~., data = train)
model_RF
model_RF$votes
model_RF$importance
varImpPlot(model_RF)

predict <- predict(model_RF,newdata = test, type = "class")
head(predict)
confusionMatrix(table(predict,test$Outcome))
library(Metrics)
accuracy(test$Outcome,predict)
library(ROCR)
library(Information)
library(InformationValue)
plotROC(predict,test$Outcome)
table(test$Outcome,predict)
str(predict)
test$pred_rf <- predict
test$pred_rf <- as.numeric(as.character(test$pred_rf))
pred_rf <- prediction(test$Outcome,test$pred_rf)
auc <- performance(pred_rf,"auc")
auc@y.values


# logistics

model_glm <- glm(Outcome~., data = train, family = "binomial")
summary(model_glm)
predict_glm <- predict.glm(model_glm,newdata = test, type = "response")
head(predict_glm)
predict_lm <- ifelse(predict_glm>0.36,1,0)
head(predict_lm)
head(df$Outcome)
table(test$Outcome,predict_lm)
accuracy(test$Outcome,predict_lm)
caret::confusionMatrix(table(test$Outcome,predict_lm))
pred <- prediction(test$Outcome,test$predicted)
test$predicted <- predict_lm
str(test$Outcome)
str(test$predicted)
test$Outcome <- as.numeric(as.character(test$Outcome))
head(test$Outcome)
perf <- performance(pred,"tpr","fpr")
plot(perf)
auc<- performance(pred,"auc")
auc@y.values
