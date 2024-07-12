library(TSA)
library(forecast)
library(tseries)
library(normtest)

library(readr)
turkey <- read_csv("turkey_elec.csv", col_names = FALSE)
View(turkey)

#Mengubah data menjadi data time series
turkeyts<-ts(turkey,frequency=365,start=2000)
head(turkeyts)
tail(turkeyts)

#Cek kestasioneran data 
plot(turkeyts) #secara subjektif
adf.test(turkeyts) #secara objektif:Uji adf

#Karena sudah stasioner, maka akan dicari model dari ARIMA-nya
tsdisplay(turkeyts)
eacf(turkeyts)

#Dari hasil plot acf, pacf, dan eacf nya, maka akan dicoba beberapa model yang kemungkinan cocok
model1 <- Arima(turkeyts, order=c(2,0,3))
model2 <- Arima(turkeyts, order=c(3,0,3))
model3 <- Arima(turkeyts, order=c(4,0,3))

#Panggil semua modelnya lalu bandingkan MLE dan AIC-nya
model1
model2
model3
cbind(model1,model2,model3)

#Modelnya adalah ARIMA(4,0,3)