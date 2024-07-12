library(stats)
library(MASS)

#import data
library(readxl)
data <- read_excel(NULL)
View(data)

#inspecting data
datamat=as.matrix(datamat)
datamat[1:5,1:5]

datamatrix=as.matrix(datamatrix)

fit<-cmdscale(datamatrix,eig=TRUE,k=2) #MDS being performed

mds<-as_tibble(fit$points) #koordinat dari objek MDS
colnames(mds)<-c('dim1','dim2')
ggscatter(mds,x='dim1',y='dim2',
          label=colnames(datamatrix),
          size=1,
          tittle='MDS: judul',
          repel=TRUE)   #plot