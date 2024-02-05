library(tseries)
library(forecast)
ap <- AirPassengers
class(ap)
start(ap)
end(ap)
frequency(ap)
summary(ap)
plot(ap)
abline(reg = lm(ap~time(ap)))  # fit line to check variance and mean of series

cycle(ap)
plot(aggregate(ap,FUN = mean)) # Will aggregate and display year on year trend

boxplot(ap~cycle(ap), col="aquamarine3", xlab="Frequncy", ylab="No of Passengers")

plot(ap)
plot(log(ap))   # log removes unequal varinace
plot(diff(log(ap)))  # Address the trend by I(1)

# AR I MA
# p  d q

acf(diff(log(ap)))  # Autocorrelation Function q=1
pacf(diff(log(ap)))  # Partial AutoCorrelation Function  p=0

fit <- arima(log(ap),c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
fit

pred <- predict(fit, n.ahead = 12*10)  # Predicting for next 10 Years

pred1 <- 2.718^pred$pred   # e value for neutralizing the logarithmic

head(pred1)

ts.plot(ap,pred1,log="y", lty=c(1,3),col=c("blue", "red"))  

# Testing the model

data <- ts(ap,frequency = 12,start = c(1949,1), end = c(1959,12))

fit1 <- arima(log(data), c(0,1,1), seasonal = list(order=c(0,1,1), period=12))

pred2 <- predict(fit1,12*10)
pred3 <- 2.718^pred2$pred

data_1960 <- head(pred3,12)
data_1960

# Comparing original and predicted 1960 passengers 
data_1960 <- round(data_1960,digits = 0)
original_1960 <- tail(ap,12)
data_1960
original_1960
ts.plot(data,pred3, log="y", lty=c(1,3))
