rm(list = ls())
#install.packages("forecast")
#install.packages("tseries")
#install.packages("ggplot2")
library(forecast)
library(tseries)
library(ggplot2)

data <- read.csv("C:/Users/demreis3/Desktop/Time_Series/data-science-toolox-master/data/international-airline-passengers.csv", header = TRUE, sep=";")
pas<-ts(data[,2], frequency = 12, start=c(1949,1))
pas
plot(pas,
     lwd=2, col="red", xlab="time",ylab="passengers",
     ylim=c(0,600))

summary(pas)
aggregate(pas)
plot(aggregate(pas))

#split time series into train and test sets 
library(xts)

pass.train <-pas[1:132]
pass.test<-pas[133:length(pas)]

pas.train<-ts(pass.train, frequency = 12, start=c(1949,1))
pas.test<-ts(pass.test, frequency = 12, start=c(1960,1))
pas.train

plot(pas.train, col = "red")
lines(pas.test, col = "blue")


#Test for Stationarity
adf.test(pas, "stationary", k=12)
acf(pas, main="ACF")
pacf(pas, main="PACF")


count_d1<-diff(pas, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = "stationary", k=12)
acf(count_d1, main="ACF for Differenced Series")
pacf(count_d1, main="PACF for Differenced Series")


#Smoothing of time series (MA)

pas_ma_12<-ma(pas, order = 12, centre=T)

plot(pas,
     lwd=2, col="black", xlab="time",ylab="passengers",
ylim=c(0,600))
lines(pas_ma_12, col="red")
legend("topleft", c("Actual","Yearly Moving Average"), 
       col=c("black", "red"),lty=c(1, 1))



#Linear Filtering

plot(pas,type="l")
pas.1 <- filter(pas,filter=rep(1/5,5))
pas.2 <- filter(pas,filter=rep(1/81,81))
lines(pas.2,col="blue")
lines(pas.1,col="red")

time(pas)
plot(pas)
abline(reg = lm(pas~time(pas)), col="red")

summary(lm(pas~time(pas)))

#Decomposition of Time Series

pasdecomposed<-decompose(pas.train)
plot(pasdecomposed)
pasdecomposed

pasdecomposed$random
plot(pasdecomposed$random)

#Seasonally Adjusting
pasSeasAdj<-pas-pasdecomposed$seasonal
plot(pasSeasAdj)


#HoltWinters Prediction

plot(HoltWinters(pas))
pas.hw<-HoltWinters(pas.train)
plot(pas.hw)
pas.predict<-predict(pas.hw, n.ahead = 1*12)
ts.plot(pas.train, pas.predict, lty=1:2, col=c("blue","green"))
lines(pas.test, col="red")


#Model Identification and Estimation

findbest<-auto.arima(pas.train)
findbest
plot(forecast(findbest,h=10))
lines(pas.test, col="red")
legend("topleft", c("Forecasting","True"), 
       col=c("blue", "red"),lty=c(1, 1))

#Create ARIMA prediction model

fitted<-arima(pas.train, order=c(2,1,1), list(order=c(0,1,0), period=12))
fitted

#Compute prediction intervals

forecast<-predict(fitted, n.ahead=10)

#Set Kodfidenz-Interval

Upper<-forecast$pred +1.96*forecast$se
Lower<-forecast$pred -1.96*forecast$se

ts.plot(pas.train, forecast$pred, Upper, Lower,pas.test, col=c(1,2,4,4,5), lty=c(1,1,2,2,1))
library(graphics)
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% prediction interval)","True"), 
       col=c(1, 2, 4,5),lty=c(1, 1, 2,1))

#Residual Analysis
#In best case there are no singificant serial correlations for ony lag
res<-residuals(fitted)
acf(res)
pacf(res)

#QQ-Plot
qqnorm(residuals(fitted))
qqline(residuals(fitted))

#stationarity
adf.test(fitted$residuals, alternative="stationary")



#Forecasting with decomposition
#This method forecast separately seasonal component and seasonaly adjusted component 

fit <- stl(pas.train, t.window=12, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",
     main="Naive forecasts of seasonally adjusted data")

fcast<-forecast(fit, method = "naive")
plot(fcast, ylab = "New order index" )



#Holt's linear trend method
#Holt and Winters extended Holt's method to capture seasonality.
#Three smoothing equations-one for the level,
#one for trend, and one for seasonality

fit1<-holt(pas.train, alpha=0.8,beta=0.2, initial = "simple", h=10)
fit2<-holt(pas.train, alpha=0.8,beta=0.2, initial = "simple", exponential=TRUE, h=10)
fit1$model$state

fitted(fit1)

fit3 <- holt(pas.train, alpha=0.8, beta=0.2, damped=TRUE, h=10) 
plot(fit2, type="o", ylab="Air passengers", xlab="Year", 
     fcol="white")
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))


#Exponential smoothing

fit5<-ets(pas.train)
fit6<-ets(pas.train, model = "ZZZ", damped = FALSE)
fcast1<-forecast(fit5,h=12)
fcast2<-forecast(fit6,h=12)

fit5
fit6
plot(fit5)
plot(fit6)
plot(forecast(pas.train))
lines(pas.test, col=2)
