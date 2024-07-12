library(readxl)
data <- read_excel("data.xlsx", col_types = c("skip", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"))
View(data)

lm.fit<-lm(C~G+I+L+H+A, data=data)
anova(lm.fit)
summary(lm.fit)
vif(lm.fit)

#Transformasi data
library(car)
lm.fit2 <- lm(C~I+L+H+A, data=data)
anova(lm.fit2)
summary(lm.fit2)
vif(lm.fit2)

library(SciViews)
lm.trans<-lm(ln(C)~ln(I)+ln(L)+ln(H)+ln(A), data=data)
anova(lm.trans)
summary(lm.trans)
vif(lm.trans)
