library(readr)
USCorporateDefault <- read_csv("USCorporateDefault.csv", 
                               col_types = cols(`Firm ID` = col_skip(), 
                                                Year = col_skip(), Default = col_number(), 
                                                `WC/TA` = col_number(), `RE/TA` = col_number(), 
                                                `EBIT/TA` = col_number(), `ME/TL` = col_number(), 
                                                `S/TA` = col_number()))
View(USCorporateDefault)
head(USCorporateDefault)

summary(USCorporateDefault)

library(dplyr)
names(USCorporateDefault)[2] <- "WC_per_TA"
names(USCorporateDefault)[3] <- "RE_per_TA"
names(USCorporateDefault)[4] <- "EBIT_per_TA"
names(USCorporateDefault)[5] <- "ME_per_TL"
names(USCorporateDefault)[6] <- "S_per_TA"

str(USCorporateDefault)

#Cek distribusi data
table(USCorporateDefault$Default)

#cek kelas distribusi
prop.table(table(USCorporateDefault$Default))

#menggunakan decision tree untuk modelling
library(rpart)
treeimb <- rpart(Default~., data=USCorporateDefault)
pred.treeimb <- predict(treeimb, newdata=USCorporateDefault)

#membuat data jadi balance
library(ROSE)
USCorporateDefault1 <- ovun.sample(Default~., data=USCorporateDefault, method='over', p=0.5)$data
table(USCorporateDefault1$Default)

USCorporateDefault2 <- ovun.sample(Default~., data=USCorporateDefault, method='under', N=4750, seed=1)$data
table(USCorporateDefault2$Default)

USCorporateDefault3 <- ovun.sample(Default~., data=USCorporateDefault, method='both', p=0.5, N=4750, seed=1)$data
table(USCorporateDefault3$Default)

library(DMwR)
USCorporateDefault <- SMOTE(Default~., USCorporateDefaul, perc.over=2375, perc.under=2375, p=0.5)
OS <- SMOTE(random_state=0, data=USCorporateDefault)

#regresi
model1<-lm(Default~WC_per_TA+RE_per_TA+EBIT_per_TA+ME_per_TL+S_per_TA, 
           data=USCorporateDefault1)
summary(model1)

model3<-lm(Default~WC_per_TA+RE_per_TA+EBIT_per_TA+ME_per_TL+S_per_TA, 
           data=USCorporateDefault3)
summary(model3)

model1$model1$fitted<-predict(model1, type='response') 
model1$model1$fitted
