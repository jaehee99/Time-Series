library(tseries)
library(forecast)
library(readr)

data <- read_csv("../data/data.csv")
head(data)
class(data)

# data.frame -> ts frame 
data_ts <- ts(data)
class(data_ts)

data_ts

# kpss.test 
kpss.test(data_ts)

# analysis: p-value = 0.03521 <0.05 so we reject H0, 
# so this is non-stationary time series

ts.plot(data_ts)

data_ts.ff1 <- diff(data_ts, differences=1)
plot.ts(data_ts.ff1)


par(mfrow=c(2,1))
acf(data_ts.ff1, main="ACF")
pacf(data_ts.ff1, main="PACF")


final_arima1 <- arima(data_ts, order = c(3,1,0))
final_arima2 <- arima(data_ts, order = c(2,1,0))
final_arima3 <- arima(data_ts, order = c(2,1,1))

final_arima1
final_arima2
final_arima3

# Choose the model that aic is the smallest among three models
# Best model is final_arima3

# model diagnosis
tsdiag(final_arima3)

# forecast 12 
forecast(final_arima3, h=12)
plot(forecast(final_arima3, h=12))
