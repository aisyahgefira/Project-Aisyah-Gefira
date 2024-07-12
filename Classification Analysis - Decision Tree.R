library(party)
library(rpart)
library(rattle)
library(caret)

#import data
library(readxl)
data <- read_excel(NULL)
View(data)
head(data)

#Pembagian data training dan data testing dengan perbandingan 80:20
n<-round(nrow(data)*0.80)
set.seed(123)
samp<-sample(1:nrow(data),n)
datatrain<-data[samp,]
datatest<-data[-samp,]

#Membentuk model klasifikasi decision tree menggunakan data training
#tentukan variabel dependen dan independennya apa
fit<-rpart(vardependennya~.,data=datatrain,method='class')
summary(fit)
fit$variable.importance
barplot(fit$variable.importance)

#Plot decision tree
fancyRpartPlot(fit)

#Prediksi data testing
prediksi<-predict(fit,newdata=datatest,type='class') #Prediksi testing
table(prediksi,datatest$vardependennya) #Confusion matrix
confusionMatrix(data=prediksi,reference=datatest$vardependennya)

#Tes Variable Importance: Mencari variabel yang penting untuk diletakkan di atas decision tree
fit$variable.importance
barplot(fit$variable.importance)