#METODE K-NEAREST NEIGHBOR

library(dplyr)
library(caret)
library(gmodels)
library(class)

#Import Data
library(readxl)
data <- read_excel("D:/ANALISIS MULTIVARIAT/data5.xlsx")
View(data)
head(data)

#Split Data
angka_acak<-sample(1:nrow(data),0.9*nrow(data))

#Fungsi Normalisasi
nor<-function(x){(x-min(x))/(max(x)-min(x))}

#Normalisasi kolom tertentu pada Data
datanor<-as.data.frame(lapply(data[,c(1,2,3)],nor))
summary(datanor)

#Extract Training Set
datatrain<-datanor[angka_acak,]
dim(datatrain)

#Extract Testing Set
datatest<-datanor[-angka_acak,]
dim(datatest)

#Extract kolom tertentu dari train dataset, digunakan sbg 'cl' dalam fungsi KKN
data_target_category<-data[angka_acak,4] #4 itu nomor kolom yg ingin di extract

#Extract kolom tertentu jika dataset uji mengukur akurasi(?)
data-test_category<-data[-angka_acak,4]

#Menjalankan fungsi KKN
fungsikkn<-knn(datatrain,datatest,cl=data_target-category$JK,k=20)
fungsikkn

#Matriks Konfusi
tab<-table(fungsikkn,data_test_category$JK)
tab

#Cek akurasi
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)