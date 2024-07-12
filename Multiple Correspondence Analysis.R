library(FactoMineR)
library(ca)
library(dplyr)

#import data
library(readxl)
data <- read_excel(NULL)
View(data)

mcamodel<-MCA(data[,-1],     #dataset keculi kolom pertama
              graph=FALSE) 
summary(mcamodel)

plot(mcamodel,cex=0.7,
     col.var='black',   #warna nama variabel
     invis='ind')      #warna indikator
dimdesc(mcamodel)