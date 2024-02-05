df <- read.csv("GOODBAD.csv", na.strings = c("","NA", "na"))
str(df)
# data cleaning 
colnames(df)[1] <- "Account_status"
colSums(is.na(df))
dplyr::filter(df,between(df$Amount, 2500,2900))
library(dplyr)

# Missing Value treatment
NA_purpose <- filter(df, between(df$Amount,2800,2900))%>%filter(NA_purpose$EmployTenure=="A75" & NA_purpose$CreditHistory=="A32")
df$Purpose[is.na(df$Purpose)] <- "A42"
df$Duration[is.na(df$Duration)] <- mean(df$Duration, na.rm = TRUE)

#Exploring data
table(df$Good_Bad)
df$Good_Bad <- ifelse(df$Good_Bad==-1,1,as.character(df$Good_Bad))
df$Good_Bad <- as.integer(as.character(df$Good_Bad))
df$Good_Bad <- ifelse(df$Good_Bad==2,0,as.character(df$Good_Bad))

str(df)
df$NumLiab <- as.factor(as.character(df$NumLiab))
df$ExCredit <- as.factor(as.character(df$ExCredit))
df$CurrResidTenure <- as.factor(as.character(df$CurrResidTenure))
df$Rate <- as.factor(as.character(df$Rate))
sapply(df,class)
str(df)
library(Information)
library(InformationValue)
create_infotables(data = df, valid=df, y="Good_Bad")
str(df$Good_Bad)

# Data splitting
library(caret)
set.seed(2)
library(caTools)

index <- sample.split(df$Good_Bad, SplitRatio = 0.70)
train <- subset(df, index==TRUE)
test <- subset(df, index==FALSE)
table(train$Good_Bad)
table(test$Good_Bad)

# Model building
model <- glm(Good_Bad~., data = df, family = "binomial")
summary(model)
model2 <- step(model, direction = "both")
model3 <- glm(Good_Bad ~ Check_Account_Status + Duration + CreditHistory + Purpose + 
                Amount + SavingsAcc + Rate + Status + Debtors + Age + Plans + 
                Hous + Tel + Foreign, data = df, family = "binomial")
summary(model3)

# creating dummies
install.packages("dummies")
df_new <- dummies::dummy.data.frame(df, sep=".")
colSums(is.na(df))
colnames(df)
library(dummies)
df_dummy <- dummy.data.frame(df, names = c("Check_Account_Status", "CreditHistory","Purpose",
                                           "SavingsAcc","Status","Debtors","Plans","Foreign"), sep=".")
df_with_dummy <- cbind(df,df_dummy)
model5 <- glm(Good_Bad~Check_Account_Status.A13+Check_Account_Status.A14+CreditHistory.A34+Purpose.A41+Purpose.A43+
                Amount+SavingsAcc.A64+SavingsAcc.A65+Rate+Status.A93+Debtors.A103+Plans.A143, data = df_with_dummy, family = "binomial")
summary(model5)

plot(df$Age,df$Good_Bad)
str(df$Good_Bad)
df$Good_Bad <- as.factor(as.character(df$Good_Bad))
summary(df$Age)

df_with_dummy$Age_Young <- ifelse(between(df$Age,19,27),1,0)
df_with_dummy$Age_Middle <- ifelse(between(df$Age,28,60),1,0)
df_with_dummy$Age_Old <- ifelse(between(df$Age,61,75),1,0)

model6 <- glm(Good_Bad~Check_Account_Status.A13+Check_Account_Status.A14+CreditHistory.A34+Purpose.A41+Purpose.A43+
                Amount+SavingsAcc.A64+SavingsAcc.A65+Rate+Status.A93+Debtors.A103+Plans.A143+Age_Middle+Age_Young+Age_Old,
              data = df_with_dummy,family = "binomial")
summary(model6)
head(model5$fitted.values)
table(df$Good_Bad)
df$predicted <- ifelse(model$fitted.values>=0.40,1,0)
str(df$Good_Bad)
str(df$predicted)
df$Good_Bad <- as.numeric(df$Good_Bad)

confusionMatrix(df$Good_Bad,df$predicted)
library(InformationValue)
library(Information)
library(Metrics)
accuracy(df$Good_Bad,df$predicted)
library(ROCR)
pred <- prediction(df$Good_Bad,df$predicted)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred,"auc")
auc@y.values
plotROC(df$Good_Bad,df$predicted)
