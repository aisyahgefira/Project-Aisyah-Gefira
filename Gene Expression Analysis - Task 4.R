install.packages("BiocManager")
BiocManager::install(version="3.16")

library(Biobase)
library(GEOquery)

dtgeo <- getGEO('GDS5026', destdir=".")
dtgeo

eset <- GDS2eSet(dtgeo, do.log2=TRUE)
eset

phdtgeo <- pData(eset)
head(phdtgeo)

expdtgeo <- exprs(eset)
dim(expdtgeo)
head(expdtgeo)

Meta(dtgeo)$platform

annotation(eset) <- "hgu133a"

BiocManager::install("hgu133a.db")
library(hgu133a.db)

BiocManager::install("genefilter")
require(genefilter)

esetfilt = nsFilter(eset)
esetfilt

expdtgeofilt <- exprs(esetfilt$eset)

par(mfrow=c(1,2))
hist(expdtgeo, main="original")
hist(expdtgeofilt, main="filtered")

vargrp <- phdtgeo[,2]
table(vargrp)

group <- ifelse(vargrp=="control (siGL3)",0,1)

BiocManager::install("limma")
library(limma)

design <- model.matrix(~group)
fit <- eBayes(lmFit(expdtgeofilt, design))
fit

topresult <- topTable(fit, coef=2, number=50)

rownames(topresult)

selected <- rownames(expdtgeofilt)%in%rownames(topresult)

expdtgeosel <- expdtgeofilt[selected,]

heatmap(expdtgeosel)

par(mfrow=c(2,2))
for(i in 1:4) plot(vargrp, expdtgeosel[i,],
                   main=rownames(expdtgeosel)[1])

library("annotate")
library("hgu133a.db")

geneselected <- select(hgu133a.db, rownames(topresult),
                       c("SYMBOL","ENTREZID","GENENAME"))
geneselected
ids <- rownames(topresult)
geneselected <- select(hgu133a.db, ids, c("SYMBOL", "ENTREZID", "GENENAME", 
                                          "GO"))
BiocManager::install("GO.db")
library(GO.db)

goselected <- select(GO.db, geneselected%GO, c("TERM", "GOID"))
head(goselected)

### CLUSTERING ###
par(mfrow=c(1,1))
pairs(expdtgeo)

install.packages("corrplot")
library(corrplot)
corrplot(cor(expdtgeo), method = "ellipse", type = "upper")

##K-MEANS##
#data(expdtgeo)
#reschaling
expdtgeoc <- scale(expdtgeo, scale=T)
#Kmeans with k=3
set.seed(123)
kMres <- kmeans(expdtgeoc, centers = 3)

#result
kMres$cluster
kMres$size

#plot cluster
pairs(expdtgeoc, col=c(1:3)[kMres$cluster],pch=16)