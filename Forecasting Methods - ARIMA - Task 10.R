library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(xts)

#Import Data
load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/Earth_Surface_Temperature.RData"))
str(t.global)
t.global<-apply.yearly(t.global, mean)
t.global
t.global<-na.omit(t.global) #Menghapus NaN
t.global

#Mengubah data menjadi data time series
tempts<-ts(t.global$Monthly.Anomaly.Global,frequency = 1, start=c(1753))
head(tempts)
tail(tempts)

#Cek kestasioneran data
plot(tempts) #secara subjektif
adf.test(tempts) #secara objektif

#Differencing data
difftemp<-diff(tempts)
plot(difftemp)
adf.test(difftemp)

#Menentukan model ARIMA terbaik
tsdisplay(difftemp)
eacf(difftemp)

model1 <- Arima(kingstimeseries, order=c(0,1,1))
model2 <- Arima(kingstimeseries, order=c(1,1,0))
model3 <- Arima(kingstimeseries, order=c(1,1,1))
cbind(model1, model2, model3)

#Penaksir Parameter
fit <- Arima(tempts, order=c(1,1,1))
fit

#Diagnostic Checking
checkresiduals(fit) #independensi residual
jb.norm.test(tempts, nrepl=2000) #uji normalitas residual

#Overfitting Data
overfit1 <- Arima(tempts, order=c(2,1,1))
overfit2 <- Arima(tempts, order=c(1,1,2))
overfit1
overfit2
cbind(fit, overfit1, overfit2)

#Cross Validation
actual=window(tempts, start=c(2013))
tempts2=window(tempts, end=c(2012))

fit2 <- Arima(tempts2, order=c(1,1,1))
forecast2 <- forecast(fit2, h=5)
plot(forecast2)
cbind(actual, forecast2)

#Forecasting
forecast <- forecast(fit, h=5)
plot(forecast)
forecast