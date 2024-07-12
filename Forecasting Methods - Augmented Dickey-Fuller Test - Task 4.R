library(TSA)
library(forecast)
library(tseries)
library(normtest)

library(readr)
shampoo<-read_csv("shampoo.csv", col_types = cols(Month = col_skip()))
View(shampoo)

#Mengubah data menjadi data time series
shampoots<-ts(shampoo,frequency=12,start=c(1,1))
head(shampoots)
tail(shampoots)

#Cek kestasioneran data 
plot(shampoots) #secara subjektif
adf.test(shampoots) #secara objektif:Uji adf

#Differencing Data
diffshampoo<-diff(shampoots,differences=1)
plot(diffshampoo)

#Memastikan kembali Kestasioneran data dengan uji adf
adf.test(diffshampoo)

tsdisplay(diffshampoo)
eacf(diffshampoo)
