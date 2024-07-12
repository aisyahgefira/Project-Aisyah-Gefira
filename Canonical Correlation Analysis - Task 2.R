install.packages('psych')
install.packages('tmvnsim')
install.packages('MVN')
install.packages('CCP')

library(psych)
library(MVN)
library(CCA)
library(CCP)
library(ggplot2)


library(readr)
fuel_cons <- read_csv("C:/Users/USER/Downloads/fuel_cons.csv", 
                      col_types = cols(`Fuel Consumption (City (L/100 km)` = col_number(), 
                                       `Fuel Consumption(Hwy (L/100 km))` = col_number(), 
                                       `Fuel Consumption(Comb (L/100 km))` = col_number(), 
                                       `Fuel Consumption(Comb (mpg))` = col_number(), 
                                       `CO2 Emissions(g/km)` = col_number(), 
                                       `CO2 Rating` = col_number(), `Smog Rating` = col_number()))
View(fuel_cons)

#Matriks Korelasi
library(boot)
P <- fuel_cons
korelasi <- P[,c(9:15)]
cor.mat <- cor(cbind(korelasi))
cor.mat

konsumsi = fuel_cons[,9:12]
emisi = fuel_cons[,13:15]
fuel = fuel_cons[,9:15]
cor(konsumsi, emisi, method = "pearson")

#uji asumsi
#Uji Asumsi Multivariat Normal
xx<-as.matrix(fuel_cons[,9:12]) #membentuk matriks nxp
center<-colMeans(xx) #titik pusat
n<-row(fuel_cons)
p<-ncol(fuel_cons)
cov<-cov(xx)
d<-mahalanobis(xx,center,cov) #jarak mahalanobis

qqplot(qchisq(ppoints(n),df=p),d,xlim=c(1,20),pch=20,col='blue',
       ylim=c(0,30),main='QQ-Plot Data',ylab='Jarak Mahalanobis')
abline (a=0,b=1,col='red') #visualisasi

#Uji Multikolinearitas
y1 = fuel_cons[,9:9]
y2 = fuel_cons[,10:10]
y3 = fuel_cons[,11:11]
y4 = fuel_cons[,12:12]
x1 = fuel_cons[,13:13]
x2 = fuel_cons[,14:14]
x3 = fuel_cons[,15:15]
model1=lm(y1+y2+y3+y4~x1+x2+x3,data=data)
vif(model1)

# multiple scatterplots
pairs(P[, c(9:12)])

#Analisis Korelasi Kanonik
#Menentukan korelasi kanonik, dugaan koef gugus x dan y, serta uji hipotesis
y<-konsumsi
x<-emisi
ccan<-candisc::cancor(x,y)
summary(ccan)

#Menentukan Korelasi Kanonik dan Korelasi Gugus Peubah Terhadap Fungsi Kanonik
res.cc<-cc(x,y) #korelasi kanonik
plot(res.cc$cor,type='b') #plot

res.cc$cor #nilai korelasi kanonik

res.cc$xcoef
res.cc$ycoef #penaksir koefisian bagi peubah x dan y

#Koordinat Bagi Variate Kanonik
res.cc$scores