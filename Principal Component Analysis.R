#Masukkan packages yang dibutuhkan
library(Matrix)
library(matlib)
library(matrixcalc)
library(expm)
library(Hmisc)
#Masukkan data
library(readxl)
data<-read_excel("D:/ANALISIS MULTIVARIAT/data.xlsx")
View(data)
#Mencari vektor mean data
y_meanvec<-matrix(colMeans(data),ncol=1)
y_meanvec
#Mencari matriks kovarians S dan matriks korelasi R
S<-cov(data)
S
R<-cor(data)
R
#Mencari nilai eigen
eigval1<-eigen(S)$values
eigval1
for (i in eigval1){
  print(i)
  print(echelon(S-i*diag(nrow(S)),reduced=TRUE))}
eigval2<-eigen(R)$values
eigval2
for(i in eigval2){
  print(i)
  print(echelon(R-i*diag(nrow(R)),reduced=TRUE))
}
#Mencari vektor eigen
eigvec1<-eigen(S)$vektors*(-1)
eigvec1
eigvec2<-eigen(R)$vektors*(-1)
eigvec2
