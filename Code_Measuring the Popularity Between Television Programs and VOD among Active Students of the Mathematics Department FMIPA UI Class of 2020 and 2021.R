library(readxl)
daftar_populasi <- read_excel("C:/Users/USER/Downloads/daftar populasi.xlsx", 
                              col_types = c("numeric", "text", "numeric", 
                                            "text", "text", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "text", "text", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "text", "text"))
View(daftar_populasi)

################################ TV ######################################

#uji apakah biaya, variasi, dan kemudahan akses menyebebkan seseorang nyaman nonton tv?
#Full Model
full=lm(tv5~tv1+tv2+tv3+tv4,data=daftar_populasi)
summary(full)

pairs(~tv1+tv2+tv3,data=daftar_populasi)
pairs(tv4~tv1+tv2+tv3,data=daftar_populasi)

plot(full)

#uji apakah biaya, variasi, kemudahan akses, dan kenyamanan menyebebkan seseorang sering nonton tv?
#Full Model
full=lm(tv5~tv1+tv2+tv3+tv4,data=daftar_populasi)
summary(full)

pairs(~tv1+tv2+tv3+tv4,data=daftar_populasi)
pairs(tv5~tv1+tv2+tv3+tv4,data=daftar_populasi)

plot(full)

#uji apakah jika ia sering nonton tv disebabkan karena dia nyaman nonton tv?
#uji normalitas
qqnorm(daftar_populasi$tv4)
qqline(daftar_populasi$tv4)

qqnorm(daftar_populasi$tv5)
qqline(daftar_populasi$tv5)

#uji korelasi spearman
cor.test(x=daftar_populasi$tv4, y=daftar_populasi$tv5, method = 'spearman')


##################################  VOD  ###################################

#uji apakah biaya, variasi, dan kemudahan akses menyebebkan seseorang nyaman nonton vod?
#Full Model
full=lm(vod4~vod1+vod2+vod3,data=daftar_populasi)
summary(full)

pairs(~vod1+vod2+vod3,data=daftar_populasi)
pairs(vod4~vod1+vod2+vod3,data=daftar_populasi)

plot(full)

#uji apakah biaya, variasi, kemudahan akses, dan kenyamanan menyebebkan seseorang sering nonton vod?
#Full Model
full=lm(vod5~vod1+vod2+vod3+vod4,data=daftar_populasi)
summary(full)

pairs(~vod1+vod2+vod3+vod4,data=daftar_populasi)
pairs(vod5~vod1+vod2+vod3+vod4,data=daftar_populasi)

plot(full)

#uji apakah jika ia sering nonton vod disebabkan karena dia nyaman nonton vod?
#uji normalitas
qqnorm(daftar_populasi$vod4)
qqline(daftar_populasi$vod4)

qqnorm(daftar_populasi$vod5)
qqline(daftar_populasi$vod5)

#uji korelasi spearman
cor.test(x=daftar_populasi$vod4, y=daftar_populasi$vod5, method = 'spearman')
