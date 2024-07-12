library(logmult)
library(vcd)
library(ca)
library(MASS)
library(dplyr)

#import data
library(readr)
mentalhealth <- read_csv("mentalhealth.csv")
View(mentalhealth)

#yang dipakai adalah kolom yearofstudy dan depression
library(readr)
mentalhealth <- read_csv("mentalhealth.csv", 
                         col_types = cols(Timestamp = col_skip(), 
                                          `Choose your gender` = col_skip(), 
                                          Age = col_skip(), `What is your 
                                          course?` = col_skip(), 
                                          `What is your CGPA?` = col_skip(), 
                                          `Marital status` = col_skip(), 
                                          `Do you have Anxiety?` = col_skip(), 
                                          `Do you have Panic attack?` = col_skip(), 
                                          `Did you seek any specialist for a treatment?` = col_skip()
                                          )
                         )
View(mentalhealth)

library(readr)
mentalhealth <- read_csv("mentalhealth.csv", 
                         col_types = cols(Timestamp = col_skip(), 
                                          `Choose your gender` = col_skip(), 
                                          Age = col_skip(),
                                          `Marital status` = col_skip(), 
                                          `What is your CGPA?` = col_skip(),
                                          `Your current year of Study` = col_skip(),
                                          `Do you have Anxiety?` = col_skip(), 
                                          `Do you have Panic attack?` = col_skip(), 
                                          `Did you seek any specialist for a treatment?` = col_skip()
                         )
)
View(mentalhealth)

#rename nama kolom
mh<-mentalhealth %>%
  rename(
    depression = `Do you have Depression?`,
    course = `What is your course?`
  )
mh

#menyamakan isi tabel
mh$yearofstudy[mh$yearofstudy=='Year 1'] = 'year 1'
mh$yearofstudy[mh$yearofstudy=='Year 2'] = 'year 2'
mh$yearofstudy[mh$yearofstudy=='Year 3'] = 'year 3'
mh$yearofstudy[mh$yearofstudy=='Year 4'] = 'year 4'

#tabel kontingensi
conttable <- table(mh$course,mh$depression)
conttable

conttable/101

#uji independensi
mhca <- ca(conttable, graph = FALSE)
summary(mhca)
print(mhca)

plot(dataca)