library(TSA)
library(forecast)
library(tseries)
library(normtest)

#Import Data
library(readr)
msft1 <- read_csv("D:/METODE PERAMALAN/msft1.csv", 
                 col_types = cols(Date = col_skip(), `Close/Last` = col_number(), 
                                  Volume = col_skip(), Open = col_skip(), 
                                  High = col_skip(), Low = col_skip()))
View(msft1)

#Mengubah data ke bentuk data time series
tsmsft1 <- ts(msft1,frequency = 365,start=c(2022,1))
head(tsmsft1)
tail(tsmsft1)

#Cek kestasioneran data
plot(tsmsft1) #secara subjektif
adf.test(tsmsft1) #secara objektif

#Differencing data
diffmsft1 <- diff(tsmsft1, differences=1)
plot(diffmsft1)

adf.test(diffmsft1) #memastikan kembali kestasioneran data

#Menentukan Model ARIMA
tsdisplay(diffmsft1)
eacf(diffmsft1)

#Dari hasil plot acf, pacf, dan eacf nya, maka akan dicoba beberapa model yang kemungkinan cocok
model1 <- Arima(tsmsft1, order=c(0,1,1))
model2 <- Arima(tsmsft1, order=c(1,1,1))
model3 <- Arima(tsmsft1, order=c(1,1,2))

model1
model2
model3
cbind(model1,model2,model3)

#Modelnya adalah ARIMA(1,1,1)
fit<-Arima(tsmsft1,order=c(1,1,1))
fit

#cek independensi residual
#h0 = residual tidak mengandung korelasi
#h1 = residual mengandung korelasi
checkresiduals(fit)

#cek normalitas residual
#h0 = residual berdistribusi normal
#h1 = residual tidak berdistribusi normal
jb.norm.test(tsmsft1,nrepl=2000)
