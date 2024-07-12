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

shampoo <- read_csv("D:/METODE PERAMALAN/shampoo.csv")

#Mengubah data menjadi data time series
shampoots<-ts(shampoo,frequency=12,start=c(1,1))
shampoots
head(shampoots)
tail(shampoots)

#Cek kestasioneran data 
plot(shampoots) #secara subjektif: ada trend, mean tidak konstan
#secara objektif:Uji adf
#hipotesis: H0:Data tidak stasioner, H1:Data Stasioner
adf.test(shampoots)
#pvalue>alpha=0.05 sehingga H0 diterima (data tidak stasioner)

#Differencing Data
diffshampoo<-diff(shampoots,differences=1)
plot(diffshampoo) #sudah tidak ada trend, mean cukup konstan
#memastikan kembali dengan uji adf
adf.test(diffshampoo)
