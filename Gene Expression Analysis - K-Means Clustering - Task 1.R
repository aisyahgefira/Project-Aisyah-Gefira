data(USArrests)
head(USArrests)

library(corrplot)
corrplot(cor(USArrests), method = "ellipse", type = "upper")

##K-MEANS##
#elbow method#
library(cluster)

install.packages("factoextra")
library(factoextra)

#create plot of number of clusters vs total within sum of squares
fviz_nbclust(USArrests, kmeans, method = "wss")

#reschaling
USArrestsc <- scale(USArrests, scale=T)

#Kmeans with k=3
set.seed(123)
kMres <- kmeans(USArrestsc, centers = 3)

#result
kMres$cluster
kMres$size

#plot cluster
pairs(USArrestsc, col=c(1:3)[kMres$cluster],pch=16)
