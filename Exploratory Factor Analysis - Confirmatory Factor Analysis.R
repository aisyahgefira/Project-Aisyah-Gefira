library(psych)
library(GPArotation)
library(foreign)
library(lavaan)

#KMO Test
KMO(data)

#Barlet test for sphericity
uji_Bartlett <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) 
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
uji_Bartlett(data)

#Menentukan Jumlah Faktor untuk di ekstrak
ev <- eigen(cor(data)) #mencari eigenvalues
ev$values
scree(data, pc=FALSE) #plot
fa.parallel(data, fa="fa") #plot

Nfacs <- 1  #Untuk 1 faktor
fit <- factanal(data, Nfacs, rotation="varimax")
print(fit, digits=1, cutoff=0.3, sort=TRUE)

loads <- fit$loadings 
fa.diagram(loads) #plot

#CONFIRMANTORY FACTOR ANALYSIS

round(cor(data[1:5]),1) #cek korelasi antar variabel

model<-'fs=~year1 + year2 + year3 + year4 + year5'
cfa2<-cfa(model,data,std.lv=TRUE)
summary(cfa2,fit.measures=TRUE,standardized=TRUE)
