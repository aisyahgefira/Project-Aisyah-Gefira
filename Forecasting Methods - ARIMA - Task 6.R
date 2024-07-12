library(TSA)
library(forecast)
library(tseries)
library(normtest)

fancy<-read.delim("fancy.txt",header=FALSE)
View(fancy)

#Mengubah data menjadi data time series
fancyts<-ts(fancy,frequency=12,start=c(1987,1))
head(fancyts)
tail(fancyts)

#Cek kestasioneran data 
plot(fancyts) #secara subjektif
adf.test(fancyts) #secara objektif:Uji adf

#Differencing Data
difffancy<-diff(fancyts,differences=1)
plot(difffancy) #sudah tidak ada trend, mean cukup konstan
#memastikan kembali dengan uji adf
adf.test(difffancy)

#Karena sudah stasioner, maka akan dicari model dari ARIMA-nya
tsdisplay(difffancy)
eacf(difffancy)

#Dari hasil plot acf, pacf, dan eacf nya, maka akan dicoba beberapa model yang kemungkinan cocok
model1 <- Arima(nybirthsts, order=c(0,1,1))
model2 <- Arima(nybirthsts, order=c(1,1,0))
model3 <- Arima(nybirthsts, order=c(2,1,0))

#Panggil semua modelnya lalu bandingkan MLE dan AIC-nya
model1
model2
model3
cbind(model1,model2,model3)

#Modelnya adalah ARIMA(2,1,0)

fit<-Arima(fancyts,order=c(2,1,0))
fit

checkresiduals(fit)
jb.norm.test(difffancy,nrepl=2000)
