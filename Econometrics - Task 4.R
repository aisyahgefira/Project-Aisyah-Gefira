library(readr)
USCorporateDefault <- read_csv("USCorporateDefault.csv", 
                               col_types = cols(`Firm ID` = col_skip(), 
                                                Year = col_skip(), Default = col_number(), 
                                                `WC/TA` = col_number(), `RE/TA` = col_number(), 
                                                `EBIT/TA` = col_number(), `ME/TL` = col_number(), 
                                                `S/TA` = col_number()))
View(USCorporateDefault)

USCorporateDefault$rank<-factor(USCorporateDefault$rank)
head(USCorporateDefault)

summary(USCorporateDefault)

library(dplyr)
names(USCorporateDefault)[2] <- "WC_per_TA"
names(USCorporateDefault)[3] <- "RE_per_TA"
names(USCorporateDefault)[4] <- "EBIT_per_TA"
names(USCorporateDefault)[5] <- "ME_per_TL"
names(USCorporateDefault)[6] <- "S_per_TA"

#Cek distribusi data
table(USCorporateDefault$Default)

#membuat data jadi balance
library(DMwR)
USCorporateDefault<-SMOTE(Default~.,USCorporateDefault,
                          perc.over=len(os_data_y[os_data_y['Default']==0])/len(os_data_X), perc.under=200)

#regresi
model1<-lm(Default~WC_per_TA+RE_per_TA+EBIT_per_TA+ME_per_TL+S_per_TA, 
           data=USCorporateDefault)
summary(model1)

model1$model1$fitted<-predict(model1, type='response') 
model1$model1$fitted

#probit
model2<-glm(admit~gre+gpa+rank, data=mydata, family=binomial(link='probit'))
summary(model2)

model2$model2$fitted<-predict(model2, type='response') 
model2$model2$fitted

#logit
model3<-glm(admit~gre+gpa+rank, data=mydata, family=binomial(link='logit'))
summary(model3)
