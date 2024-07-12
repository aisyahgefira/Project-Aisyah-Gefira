library(TSA)
library(forecast)
library(tseries)
library(normtest)

#Import Data
rainfall<-c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall

#Mengubah data menjadi data time series
rainfallts <- ts(rainfall, frequency=12, start=c(2012,1))
head(rainfallts)
tail(rainfallts)

#Cek kestasioneran data
adf.test(rainfallts)

#Differencing data
diffrainfall <- diff(rainfallts, differences=1)
plot(diffrainfall)

adf.test(diffrainfall) #memastikan kembali kestasioneran data

#Menentukan model ARIMA
tsdisplay(diffrainfall)
eacf(diffrainfall)

model1 <- Arima(rainfallts, order=c(1,1,0))
model2 <- Arima(rainfallts, order=c(0,1,1))
model3 <- Arima(rainfallts, order=c(1,1,1))
cbind(model1, model2, model3)

fit <- Arima(rainfallts, order=c(1,1,1))
fit

checkresiduals(fit)
jb.norm.test(rainfallts, nrepl=2000