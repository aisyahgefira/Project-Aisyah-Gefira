install.packages("BiocManager")
BiocManager::install(version="3.16")

library(Biobase)
library(GEOquery)

dtgeo <- getGEO('GDS5026', destdir=".")
dtgeo

###EXPRESSION SET####
eset <- GDS2eSet(dtgeo, do.log2 = TRUE)
head(eset)

###EXPRESSION DATA####
expdtgeo <- exprs(eset)
dim(expdtgeo)
head(expdtgeo)

Meta(dtgeo)$platform

annotation(eset) <- "hgu133a"

#BiocManager::install("hgu133a.db")
#library(hgu133a.db)

###FILTERING###
#BiocManager::install("genefilter")
require(genefilter)

esetfilt = nsFilter(eset)
esetfilt

expdtgeofilt <- exprs(esetfilt$eset)
dim(expdtgeofilt)

par(mfrow=c(1,2))
hist(expdtgeo, main="original")
hist(expdtgeofilt, main="filtered")

###EXPLORATORY DATA ANALYSIS###
##Correlation Plot##
par(mfrow=c(1,1))
pairs(expdtgeofilt)

install.packages("corrplot")
library(corrplot)

corrplot(cor(expdtgeofilt), method = "ellipse", type = "upper")
ftable(cor(expdtgeofilt))

###CLUSTERING###
##HIERARCHICAL CLUSTERING##
library(tidyverse)

install.packages("factoextra")
library(factoextra)

hclust_matrix <- as.matrix(expdtgeofilt)
head(hclust_matrix)

hclust_matrix <- t(scale(t(hclust_matrix)))
dim(hclust_matrix)

gene_dist <- dist(hclust_matrix)
gene_hclust <- hclust(gene_dist, method = "complete")

#Dendrogram
plot(gene_hclust, labels = FALSE)
head(cutree(gene_hclust, k = 5))

BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = FALSE)

##K-MEANS##
kmeans_matrix <- as.matrix(expdtgeofilt)

head(kmeans_matrix)

kmeans_matrix <- t(scale(t(kmeans_matrix)))
dim(kmeans_matrix)

gene_dist <- dist(kmeans_matrix)

gene_kmeans <- kmeans(scale(kmeans_matrix), center=2)

head(gene_kmeans$cluster)

fviz_cluster(gene_kmeans, data = kmeans_matrix,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#For samples
kmeans_matrix2 <- as.matrix(t(expdtgeofilt))

kmeans_matrix2 <- scale(kmeans_matrix2)
dim(kmeans_matrix2)

gene_dist2 <- dist(kmeans_matrix2)

gene_kmeans2 <- kmeans(scale(kmeans_matrix2), center=2)

head(gene_kmeans2$cluster)

fviz_cluster(gene_kmeans2, data = kmeans_matrix2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


##Filtered Genes Clustering and Biplot##

#Filtering to top 100 based on variance
genes <- expdtgeofilt
genes.var <- apply(expdtgeofilt, 1, var)
genes.var.select <- order(-genes.var)[1:100]
expdtgeofilt.s <- expdtgeofilt[genes.var.select, ]
expdtgeofilt.d.s <- dist(t(expdtgeofilt.s))
dim(expdtgeofilt.s)

#Clustering the Filtered Genes with K-Means
kmeans_matrix3 <- as.matrix(expdtgeofilt.s)
head(kmeans_matrix3)

kmeans_matrix <- t(scale(t(kmeans_matrix3)))
dim(kmeans_matrix3)

gene_dist3 <- dist(kmeans_matrix3)

gene_kmeans3 <- kmeans(scale(kmeans_matrix3), center=2)
head(gene_kmeans3$cluster)

fviz_cluster(gene_kmeans3, data = kmeans_matrix3,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#Biplot
pca <- prcomp(expdtgeofilt.s, scale=T)
head(pca$x)

biplot(pca,
       col = c('darkblue', 'red'))

biplot(pca,
       col = c('darkblue', 'red'),
       scale = 0, xlabs = rep("*", 100))

biplot(pca,
       col = c('darkblue', 'red'),
       scale = 0, ylabs = rep("#", 8))

biplot(pca,
       col = c('darkblue', 'red'),
       scale = 0, xlabs = rep("*", 100),
       ylabs = rep("#", 8))


#Biplot with Cluster
plot(fviz_pca_biplot(pca, label="var", 
                     habillage=as.factor(gene_kmeans3$cluster)) + ggtitle("") +
       theme(text = element_text(size = 15), 
             panel.background = element_blank(), 
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             axis.line = element_line(colour = "black"),
             legend.key = element_rect(fill = "white")))
