install_github("vqv/ggbiplot")
library(devtools)
library(readr)
X2015 <- read_csv("D:/ANALISIS MULTIVARIAT/2015.csv", 
                  col_types = cols(Country = col_skip(), Region = col_skip(), 
                                   `Happiness Rank` = col_skip(), `Happiness Score` = col_number(), 
                                   `Standard Error` = col_skip(), `Economy (GDP per Capita)` = col_number(), 
                                   Family = col_skip(), `Health (Life Expectancy)` = col_number(), 
                                   Freedom = col_number(), `Trust (Government Corruption)` = col_number(), 
                                   Generosity = col_skip(), `Dystopia Residual` = col_skip()))
View(X2015)

#mengubah ke bentuk matriks
matrix<-as.matrix.data.frame(X2015)
matrix

#svd
y<-svd(matrix)
y

u<-y$u
u

L<-diag(y$d)
L

A<-y$v 
A

G<-U%*%sqrt(L)
Ht<-L%*%t(A)
H<-t(Ht)

#Membuat Biplot
G2<-G[,1:2]
H2<-H[,1:2]
biplot(G2,H2)

model1<-princomp(matrix,cor=T) 
summary(model1)
model2<-prcomp(matrix,scale=T)
summary(model2)

biplot(model1)
biplot(model2)

#keragaman
ragam<-eigen(t(matrix)%*%matrix)$values
dim1<-sum(ragam[1])/sum(ragam)*100
dim2<-sum(ragam[2])/sum(ragam)*100
dim1+dim2