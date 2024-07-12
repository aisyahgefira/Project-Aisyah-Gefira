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
library(hgu133a.db)

###FILTERING###
#BiocManager::install("genefilter")
require(genefilter)

esetfilt = nsFilter(eset)
esetfilt

expdtgeofilt <- exprs(esetfilt$eset)

# Lasso Classification----

X <- t(expdtgeofilt) #1, 2, 3, 4 for Control and 5, 6, 7, 8 for ERK5

dim(X)
rownames(X)
y <- data.frame(label = c(1, 1, 1, 1, 0, 0, 0, 0))

set.seed(123)

X <- as.matrix(X)
y <- as.matrix(y)

### select learningset
ratio <- 2/3
set.seed(123)
learnind <- sample(length(y), size=floor(ratio*length(y)))
### run L1 penalized logistic regression (no tuning)
install.packages("glmpath")
library(glmpath)
lassoresult <- LassoCMA(X=X, y=y, learnind=learnind, norm.fraction = 0.2)
show(lassoresult)
ftable(lassoresult)
plot(lassoresult)