library(forecast)
library(fpp2)
library(tseries)
library(MLmetrics)
library(ggplot2)
library(stats)

head(gas)
str(gas)
frequency(gas)
plot.ts(gas, main="Raw data plotted")
monthplot(gas, main="Monthplot")
seasonplot(gas, main="Seasonplot")
boxplot(gas~cycle(gas))

#Cleaning the data
TSgasdata=tsclean(gas)

#Saving as Time series data
TSGas <-ts(gas, frequency=12, start=c(1956,1))
summary(TSGas)

#Decompose the data
GasDec<-stl(TSGas, s.window='p') 
plot(GasDec)
GasDec$time.series

#Checking the periodicity of the data
acf(TSGas)
acf(TSGas, lag.max = 24)
pacf(TSGas, lag.max = 24)

#Checking if the data is stationary
adf.test(TSGas, alternative = "stationary")

#Deseasonalize the data
DeseasonGas <- (GasDec$time.series[,2]+GasDec$time.series[,3])
ts.plot(DeseasonGas, TSGas, col=c("red", "blue"), main="Comparison of GasData and Deseasonalized GasData")
deseasonal_gas=seasadj(GasDec)

#Differencing the time series data
count_diff1 = diff(deseasonal_gas, differences = 1)
plot(count_diff1)
adf.test(count_diff1, alternative = "stationary")

#acf and pacf for dif time series
Acf(count_diff1, main='ACF for Differenced Series')
Pacf(count_diff1, main='PACF for Differenced Series')

#Splitting into training and test sets

GasdataTrain <- window(count_diff1, start=c(1970,1), end=c(1982,9), frequency=12)
GasdataTest <- window(count_diff1, start=c(1982,10), frequency=12)

#Auto ARIMA

AutoArimaGasTrain=auto.arima(GasdataTrain,seasonal=TRUE)
AutoArimaGasTrain
MAPE(AutoArimaGasTrain$fitted,AutoArimaGasTrain$x)

acf(AutoArimaGasTrain$residuals)
pacf(AutoArimaGasTrain$residuals)

Box.test(AutoArimaGasTrain$residuals, lag = 30, type = "Ljung-Box")

GasAutoArimaForecast=forecast(AutoArimaGasTrain)
plot(GasAutoArimaForecast)
accuracy(GasAutoArimaForecast)
vec.autoarima=cbind(GasAutoArimaForecast$mean,GasdataTest)
ts.plot(vec.autoarima,col=c("blue", "red"))

#Manual Arima

ManualArimaGasTrain<-arima(GasdataTrain, order = c(0,0,3), season=list(order = c(1,0,2), period=12))
ManualArimaGasTrain
MAPE(ManualArimaGasTrain$fitted,ManualArimaGasTrain$x)

acf(ManualArimaGasTrain$residuals)
pacf(ManualArimaGasTrain$residuals)

Box.test(ManualArimaGasTrain$residuals, lag = 30, type = "Ljung-Box")

GasManualArimaForecast=forecast(ManualArimaGasTrain)
plot(GasManualArimaForecast)
accuracy(GasManualArimaForecast)
vec.manualarima=cbind(GasManualArimaForecast$mean,GasdataTest)
ts.plot(vec.manualarima,col=c("blue", "red"))

