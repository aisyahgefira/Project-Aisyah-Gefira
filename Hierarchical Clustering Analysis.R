#LOAD LIBRARY & INPUT DATA
library(dplyr) #data processing
library(DT)
data <- read_excel("D:/ANALISIS MULTIVARIAT/data.xlsx")
View(data)
datatable(data, 
          caption="Monthly Sales of Product A For A Plastic Manufacturer for 
          years 1 through 5")

#NORMALISASI DATA
data_stndr<-scale(data[,2:6]) #memilih data indikator tahun

#MENGHITUNG MATRIKS DISTANCE
d<-dist(x=data_stndr,method="euclidean")
d

#ANALISIS CLUSTER HIERARKI

#METODE 1: SINGLE LINKAGE
hc_single<-hclust(d=d,method="single")
#MEMBUAT DENDOGRAM
plot(hc_single,hang=-1)

#METODE 2: COMPLETE LINKAGE
hc_complete<-hclust(d=d,method="complete")
#MEMBUAT DENDOGRAM
plot(hc_complete,cex=0.6,hang=-1)
abline(h=3,col="red")

#MENGINTERPRETASI CLUSTER YANG TERBENTUK
cut_point=cutree(hc_complete,k=2) #memilih 2 cluster
data %>%
  mutate(cluster=cut_point) %>%
  group_by(cluster) %>%
  summarise(mean_year1=round(mean(year1),2),
            mean_year2=round(mean(year2),2),
            mean_year3=round(mean(year3),2),
            mean_year4=round(mean(year4),2),
            mean_year5=round(mean(year5),2))
