library(TSA)
library(forecast)
library(tseries)
library(normtest)

nybirths<-read.delim("nybirths.txt",header=FALSE)
View(nybirths)

#Mengubah data menjadi data time series
nybirthsts<-ts(nybirths,frequency=11,start=c(1946,1))
head(nybirthsts)
tail(nybirthsts)

#Cek kestasioneran data 
plot(nybirthsts) #secara subjektif
adf.test(nybirthsts) #secara objektif:Uji adf

#Karena sudah stasioner, maka akan dicari model dari ARIMA-nya
tsdisplay(nybirthsts)
eacf(nybirthsts)

#Dari hasil plot acf, pacf, dan eacf nya, maka akan dicoba beberapa model yang kemungkinan cocok
model1 <- Arima(nybirthsts, order=c(1,0,0))
model2 <- Arima(nybirthsts, order=c(0,0,1))
model3 <- Arima(nybirthsts, order=c(2,0,0))

#Panggil semua modelnya lalu bandingkan MLE dan AIC-nya
model1
model2
model3
cbind(model1,model2,model3)

#Modelnya adalah ARIMA(2,0,0)