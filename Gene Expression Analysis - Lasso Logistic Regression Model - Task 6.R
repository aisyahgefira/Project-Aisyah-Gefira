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


# Lasso Classification----

X <- t(expdtgeofilt) #1, 2, 3, 4 for Control and 5, 6, 7, 8 for ERK5

dim(X)
rownames(X)
y <- data.frame(label = c(1, 1, 1, 1, 0, 0, 0, 0))

#install.packages("glmnet")
#install.packages("pls")
#install.packages("boot")
library(glmnet)
library(pls)
library(boot)

set.seed(123)

X <- as.matrix(X)
y <- as.matrix(y)

# Fit lasso logistic regression model
lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)

# Plot the cross-validated mean deviance
plot(lasso_model)

# Get the optimal lambda value
optimal_lambda <- lasso_model$lambda.min
cat("Optimal Lambda:", optimal_lambda, "\n")

# Extract the coefficients for the optimal lambda
lasso_coef <- as.matrix(coef(lasso_model, s = optimal_lambda))

# Create a data frame with variable names and coefficients
lasso_results <- data.frame(
  Variable = rownames(lasso_coef),
  Coefficient = as.numeric(lasso_coef)
)

# Filter out rows with zero coefficients
nonzero_results <- lasso_results[lasso_results$Coefficient != 0, ]

# Sort the data frame by the absolute value of coefficients
sorted_results <- nonzero_results[order(-abs(nonzero_results$Coefficient)), ]

# Print the sorted non-zero coefficients with variable names
cat("Lasso Coefficients (sorted by magnitude):\n")
print(sorted_results)
