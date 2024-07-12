library(CA)
library(FactoMineR)
library(factoextra)
library(Hmisc)
library(devtools)

studentsperformance <- read_csv("D:/ANALISIS MULTIVARIAT/StudentsPerformance.csv")

#rename nama kolom
students<-studentsperformance %>%
  rename(
    parents = `parental level of education`,
    mathscore = `math score`
  )
students

#tabel kontingensi
conttable <- table(students$mathscore, students$parents)
conttable

#matriks korespondensi
conttable/1000

#uji independensi
studentsca <- ca(conttable, graph = FALSE)
studentsca <- CA(conttable, graph = FALSE)
print(studentsca)

#visualisasi
fviz_ca_biplot(studentsca, repel = TRUE)
