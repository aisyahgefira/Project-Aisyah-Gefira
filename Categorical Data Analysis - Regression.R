#Data tabel 3.8 untuk logistic regn di Tabel 5.3
#baseline=alcohol consumption 0
#Menghitung banyaknya observasi di tiap kategori
tot0<-sum(48,17066)
tot1<-sum(38,14464)
tot12<-sum(5,788)
tot35<-sum(1,126)
tot6<-sum(1,37)

#Membuat variabel dummy untuk masing-masing kategori; 
#base-level= 0 alcohol consumption
i0<-c(rep(1,tot0),rep(0,tot1+tot12+tot35+tot6))
i1<-c(rep(0,tot0), rep(1,tot1),rep(0, tot12+tot35+tot6))
i12<-c(rep(0,tot0+tot1), rep(1,tot12),rep(0, tot35+tot6))
i35<-c(rep(0,tot0+tot1+tot12), rep(1,tot35),rep(0, tot6))
i6<-c(rep(0,tot0+tot1+tot12+tot35), rep(1,tot6))

#Membuat variabel target y; 
#1=malformation present; 0=malformation absent

y<-c(rep(1,48), rep(0, 17066), rep(1, 38), rep(0,14464), 
     rep(1,5), rep(0,788), rep(1,1), rep(0,126), 
     rep(1,1), rep(0,37))

mydat<-data.frame(i0,i1,i12,i35,y)
dim(mydat)
View(mydat)

#regresi logistik
reg1<-glm(y~i0+i1+i12+i35, family="binomial",data=mydat)
summary(reg1)

exp(coef(reg1)["i12:i35"])


# TUGAS:
# 1) Lakukan regresi logistik untuk melihat "efek" dari
#    alcohol consumption terhadap ada/tidaknya malformation,
#    dengan kelompok baseline adalah kategori >=6. 
#    Interpretasikan hasilnya.
# 2. Hitunglah proporsi malformed di masing-masing 
#    kategori alcohol consumption. 
# 3. Bagaimana perbandingan odds terjadinya malformed pada anak yang dilahirkan
#    dari kelompok ibu yang  mengkonsumsi alkohol 1-2 gelas/hari dengan kelompok
#    ibu yang mengkonsumsi alkohol 3-5 gelas/hari?
# 4. Bagaimana perbandingan odds tidak terjadi malformed pada anak yang dilahirkan
#    dari kelompok ibu yang tidak mengkonsumsi alkohol dengan kelompok ibu yang mengkonsumsi
#    <1 gelas per/hari? Jelaskan.


###I*2 contingency table; Table 5.4
# Logit model untuk alcohol consumption dan kaitannya dengan malformed pada anak yang dilahirkan
# namun jika alkohol consumption dianggap berskala numerik
# no alkoloh consumption; <1 --> nilai 0.5; 1-2--> nilai 1.5; 
# 3-5--> nilai 4; >=6 --> nilai 7

#pembentukan variabel
x1<-rep(0, tot0)
x2<-rep(0.5, tot1)
x3<-rep(1.5, tot12)
x4<-rep(4, tot35); 
x5<-rep(7, tot6)
x<-c(x1,x2,x3,x4,x5)
new.dat<-data.frame(x,y)

#regresi logistik
reg2<-glm(y~x, family="binomial",data=new.dat)
summary(reg2)


# Tabel 5.6; regresi logistik berganda
# race=1 for white; 0 for black
# azt=1 for yes; 0 for no
# Pembentukan variabel penjelas

race<-c(rep(1,14+93+32+81), rep(0,11+52+12+43))
azt<-c(rep(1,14+93), rep(0,32+81), rep(1, 11+52), rep(0,12+43))

#Pembentukan variabel target, y=1 jika symptoms=yes; 0 jika no.
y<-c(rep(1,14),rep(0,93),rep(1,32),rep(0,81),
     rep(1,11),rep(0,52), rep(1,12), rep(0,43))

dat3<-data.frame(race, azt,y)
View(dat3)

#Regresi logistik berganda, dengan dua peubah penjelas
reg3<-glm(y~race+azt, family="binomial",data=dat3)
summary(reg3)

tabel<-array(data=c(14,32,93,81,11,12,52,43),
             dim=c(2,2,2),
             dimnames=list('azt'=c('yes','no'),
                           'symptoms'=c('yes','no'),
                           'race'=c('white','black')))
tabel

reg<-glm(y~symptoms+race+azt, family="binomial",data=dat3)
summary(reg)

exp(coef(reg3)[""])

#TUGAS
# 1. Karakteristik yang seperti apakah yang memiliki peluang paling tinggi
#    untuk mengalami symptom?
# 2. Berapakah perbandingan odds seseorang yang berkulit putih namun tidak 
#    menggunakan AZT untuk tidak mengalami symptom, dibandingkan dengan seseorang
#    yang berkulit hitam dan menggunakan AZT? Apa maknanya?

#Horseshoe crab data; Table 4.3 di buku
#Data source:https://users.stat.ufl.edu/~aa/cda/data.html

library(gdata)
setwd("D:/Documents") #sesuaikan dengan working directory masing-masing
dt<-read.csv("Tabel4_3.csv", header=TRUE) #ini data ambil dari data source di atas
head(dt)
attach(dt)

# recode/kategorisasi variabel
y.bin <- as.factor(ifelse(satell > 0, 1, 0))
table(color)
color1 <- as.factor(ifelse(color > 2, 0, 1))
color2 <- as.factor(ifelse(color == 3, 1, 0))
color3 <- as.factor(ifelse(color ==4, 1, 0))

#table(spine)
spine1<- as.factor(ifelse(spine ==1, 1, 0))
spine2<- as.factor(ifelse(spine ==2, 1, 0))

dat4<-data.frame(y.bin,weight,width,color1,color2,color3,spine1, spine2)
reg4<-glm(y.bin~., family="binomial",data=dat4)
summary(reg4)

# Variable selection 
#Model (C+S+W) Tabel; 6.2
 reg5<-glm(y.bin~width+color1+color2+color3+spine1+spine2, family="binomial",data=dat4)
 summary(reg5)

#Tugas
# 1. Buat regresi untuk model (C+S+C*S). Interpretasikan hasilnya.
# 2. Buat regresi untuk model (C+S+W+C*S+S*W). INterpretasikan hasilnya.
# 3. Bandingkan hasil model pada soal 1. dan soal 2. Model mana yang kalian rekomendasikan?





