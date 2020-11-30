#ALY6050_Project3_Gujrathi_D
 
#Installing package "quantmod" for getting the stock values
install.packages("quantmod")
library(quantmod)

#Assiging the start dates
start <- as.Date('2017-10-15')
#Assiging the end dates
end <- as.Date('2018-04-16')
#Calling the stock prices of Honeywell
getSymbols("HON", src = "yahoo", from = start, to = end)
#Determining the classes of the dataset
class(HON)
#Determinig the first values of the set 
head(HON)
#Assigning a vector to the column
honeywell <- HON$HON.Close
honeywell
#Plotting the stock price chart
plot(honeywell)
#Assigning the training values
honeywell.train <- window(honeywell, end = as.Date('2018-03-16'))
honeywell.train
#Assigning the testing values
honeywell.test <- window(honeywell, start = as.Date('2018-03-15'))
honeywell.test

#Part 1
#SES Method
#Installing Package "Forecast"
install.packages("forecast")
#Installing Package "Metrics"
install.packages("Metrics")
library(forecast)
library(Metrics)
#Performing the Simple Exponential Smoothing for alpha = 0.15
ses.honeywell1 <- ses(honeywell.train, alpha = 0.15, h = 10)
#Plotting Simple Exponential Smoothing for alpha = 0.15
autoplot(ses.honeywell1)
#Determining the MSE for alpha = 0.15
ses.honeywell1$model$mse
ses.honeywell1
#Performing the Simple Exponential Smoothing for alpha = 0.35
ses.honeywell2 <- ses(honeywell.train, alpha = 0.35, h = 10)
#Plotting Simple Exponential Smoothing for alpha = 0.35
autoplot(ses.honeywell2)
#Determining the MSE for alpha = 0.35
ses.honeywell2$model$mse
#Performing the Simple Exponential Smoothing for alpha = 0.55
ses.honeywell3 <- ses(honeywell.train, alpha = 0.55, h = 10)
#Plotting Simple Exponential Smoothing for alpha = 0.55
autoplot(ses.honeywell3)
#Determining the MSE for alpha = 0.55
ses.honeywell3$model$mse
#Plotting Simple Exponential Smoothing for alpha = 0.75
ses.honeywell4 <- ses(honeywell.train, alpha = 0.75, h = 10)
#Plotting Simple Exponential Smoothing for alpha = 0.75
autoplot(ses.honeywell4)
#Determining the MSE for alpha = 0.75
ses.honeywell4$model$mse
ses.honeywell4$mean
#Part 2
#HOLT's Method
#Assigning the values for beta and keeping the alpha value constant
b1 <- holt(honeywell.train, alpha = 0.75, beta = 0.15, h = 1)
b1
b1$mean
#Plotting the Holt's method plot for beta = 0.15
autoplot(b1)
#Determining the MSE for beta = 0.15
b1$model$mse
#Assigning the values for beta and keeping the alpha value constant
b2 <- holt(honeywell.train, alpha = 0.75, beta = 0.25, h = 1)
b2
#Plotting the Holt's method plot for beta = 0.25
autoplot(b2)
#Determining the MSE for beta = 0.25
b2$model$mse
#Assigning the values for beta and keeping the alpha value constant
b3 <- holt(honeywell.train, alpha = 0.75, beta = 0.45, h = 1)
b3
#Plotting the Holt's method plot for beta = 0.45
autoplot(b3)
#Determining the MSE for beta = 0.45
b3$model$mse
#Assigning the values for beta and keeping the alpha value constant
b4 <- holt(honeywell.train, beta = 0.85, h = 1)
b4
#Plotting the Holt's method plot for beta = 0.85
autoplot(b4)
#Determining the MSE for beta = 0.85
b4$model$mse

#Part 3
#Performing Simple Regression Analysis
mod <- lm(Close ~ Period, data = Honeywell)
mod
plot(mod)
mean(mod$fitted.values)
#Determining the coefficients of corelations and determinations 
summary(mod)
#Determning the residual values
mod$residuals
#Plotting a histogram of regression residuals
plot(mod$residuals)
abline(lm(Close ~ Period, data = Honeywell), col = "red")
hist(mod$residuals)
#Performing the Chi-squared test
x <- abs(mod$residuals)
x
chisq.test(x)
#Plotting a normal probability plot of the residuals
plot(mod)
#Plotting a ascatter plot of residuals vs time
plot(mod$residuals, Honeywell$Date)
#Plotting a scatter plot of residuals vs predicted stock values
plot(mod$residuals, Honeywell$Close)
