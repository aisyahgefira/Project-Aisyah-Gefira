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

library(car)
vif(model1)

#regresi
model1<-lm(Default~WC_per_TA+RE_per_TA+EBIT_per_TA+ME_per_TL+S_per_TA, 
           data=USCorporateDefault)
summary(model1)

model1$model1$fitted<-predict(model1, type='response') 
model1$model1$fitted