install.packages('TSA')
install.packages('forecast')
install.packages('tseries')
install.packages('normtest')
install.packages("hrbrthemes")

library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(plotly)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

suhu<-read_csv("suhu.csv")

#Mengubah data menjadi data time series
suhuts<-ts(suhu,frequency=7,start=c(1981,1))
head(suhuts)
tail(suhuts)

#Cek kestasioneran data 
plot(suhuts) #secara subjektif: tidak ada trend, mean konstan
#secara objektif:Uji adf
#hipotesis: H0:Data tidak stasioner, H1:Data Stasioner
adf.test(suhuts)
#pvalue<alpha=0.05 sehingga H0 ditolak (data stasioner)

