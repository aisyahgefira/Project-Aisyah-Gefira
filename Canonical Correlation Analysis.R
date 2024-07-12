library(CCA)
library(candisc)
library(GGally)
library(car)

#Import Data
library(readxl)
data5 <- read_excel("D:/ANALISIS MULTIVARIAT/data5.xlsx")
View(data5)
head(data5)
str(data5)

#Mendefinisikan Gugus Peubah X dan Y
x<-data5[,3:4]
y<-data5[,1:2]
data<-cbind(y,x)

#Visualisasi Gugus Peubah X dan Y
ggpairs(x)
ggpairs(y)

#Uji Asumsi Multivariat Normal
xx<-as.matrix(data) #membentuk matriks nxp
center<-colMeans(xx) #titik pusat
n<-row(data)
p<-ncol(data)
cov<-cov(xx)
d<-mahalanobis(xx,center,cov) #jarak mahalanobis

qqplot(qchisq(ppoints(n),df=p),d,xlim=c(1,20),pch=20,col='blue',
       ylim=c(0,30),main='QQ-Plot Data',ylab='Jarak Mahalanobis')
abline (a=0,b=1,col='red') #visualisasi

#Uji Multikolinearitas
model1=lm(y1+y2~x1+x2,data=data)
vif(model1)

#Menghitung Korelasi Gugus Peubah X, Y, dan XY
correl<-matcor(x,y)
correl #matriks korelasi

img.matcor(correl,type=2) #visualisasi

#Analisis Korelasi Kanonik
#Menentukan korelasi kanonik, dugaan koef gugus x dan y, serta uji hipotesis
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