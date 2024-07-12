library(TSA)
library(forecast)
library(tseries)
library(normtest)

sunspots<-read_csv("sunspots.csv", col_types = cols(Month = col_skip()))
View(sunspots)

#Mengubah data menjadi data time series
sunspotsts<-ts(sunspots,frequency=12,start=c(1749,1))
head(sunspotsts)
tail(sunspotsts)

#Cek kestasioneran data 
plot(sunspotsts) #secara subjektif: tidak ada trend, mean konstan
#secara objektif:Uji adf
#hipotesis: H0:Data tidak stasioner, H1:Data Stasioner
adf.test(sunspotsts)
#pvalue<alpha=0.05 sehingga H0 ditolak (data stasioner)
