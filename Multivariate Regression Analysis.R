#Uji Hipotesis Full Model
#hipotesis h0:betha=0, h1:betha tidak sama dengan nol
#taraf signifikansi 0.05
#aturan penolakan: tolak h0 jika pvalue<alpha atau fhitung>ftabel

#Import Data
library(readxl)
fish1 <- read_excel("D:/ANALISIS MULTIVARIAT/data5.xlsx")
View(fish1)
head(fish1)

#Full Model
full=lm(weight~length1+length2+length3+height+width,data=fish1)
summary(full)

pairs(~length1+length2+length3+height+width,data=fish1)
pairs(weight~length1+length2+length3+height+width,data=fish1)

plot(full)


#Uji Hipotesis Parsial
#hipotesis h0:betha1=0, h1:betha1 tidak sama dengan nol (terus sampai betha n)
#taraf signifikansi 0.05
#tolak h0 jika pvalue<alpha atau thitung>ttabel

#Import Data
library(readxl)
fish1 <- read_excel("D:/ANALISIS MULTIVARIAT/data5.xlsx")
View(fish1)
head(fish1)

model<-lm(weight~length1+length2+length3+height+width,data=fish1)
summary(model,test='wilks')