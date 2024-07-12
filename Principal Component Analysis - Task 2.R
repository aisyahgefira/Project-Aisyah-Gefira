#JAWABAN 1C

#Akan dilakukan Analisis Komponen Utama

#Langkah 1
#Masukkan packages dan data yang dibutuhkan
library(Matrix)
library(matlib)
library(matrixcalc)
library(expm)
library(Hmisc)

#MAsukkan data
nutrient1 <- read.csv("C:/Users/USER/Downloads/nutrient1.csv.xls.xls", sep=";")
View(nutrient1)
data<-nutrient1[,3:23] #Kita menggunakan 20 variabel awal

#Langkah 2
#Akan dicari Vektor Mean
y_meanvec<-matrix(colMeans(data),ncol=1)
y_meanvec

#Langkah 3
#Mencari matriks kovarians S 
S<-cov(data)
S

#Langkah 4
#Dilakukan centering, yaitu mengurangi setiap elemen matriks dengan mean yang
#bersesuaiaan
center_apply<-function(x){apply(x,2,function(y)y-mean(y))}
center_apply(data)

#Langkah 5
#Akan dicari nilai eigen untuk matriks kovariansi. Nilai eigen ini yang mewakili
#variabel yang dijelaskan pada komponen utama. Selajutkan dilakukan transformasi
#untuk mencari perspektif sedemikian data terjelaskan dengan dimensi yang lebih
#kecil

#Mencari nilai eigen
eigval<-eigen(S)$values
eigval

#Langkah 6
#Menentukan vektor eigen ternormalisasi yang bersesuaian dengan nilai eigen

#Mencari vektor eigen
eigvec<-eigen(S)$vectors*(-1)
eigvec

#Normalisasi vektor eigen
datan <- scale(x = data)

#Langkah 7
#Transformasi data ke komponen utama dengan z=Ay' dimana y adalah matriks data
#dan A adalah matriks yang kolomnya berisi vektor eigen yang telah dinormalisasi
z<-data*t(datan)
z
