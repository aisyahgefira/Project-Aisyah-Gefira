library(TSA)
library(forecast)
library(tseries)
library(normtest)

library(readr)
births <- read_csv("births.csv", col_types = cols(Date = col_skip()))
View(births)

#Mengubah data menjadi data time series
birthsts<-ts(births,frequency=30,start=c(01,1))
head(birthsts)
tail(birthsts)

#Cek kestasioneran data 
plot(birthsts) #secara subjektif
adf.test(birthsts) #secara objektif:Uji adf
?ts
#Karena sudah stasioner, maka akan dicari model dari ARIMA-nya
tsdisplay(birthsts)
eacf(birthsts)

#Dari hasil plot acf, pacf, dan eacf nya, maka akan dicoba beberapa model yang kemungkinan cocok
model1 <- Arima(birthsts, order=c(0,1,1))
model2 <- Arima(birthsts, order=c(1,1,0))
model3 <- Arima(birthsts, order=c(1,1,1))

#Panggil semua modelnya lalu bandingkan MLE dan AIC-nya
model1
model2
model3
cbind(model1,model2,model3)

#Modelnya adalah ARIMA(1,1,1)