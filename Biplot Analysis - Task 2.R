library(tibble)
library(ellipse)

#import data
library(readxl)
data <- read_excel(NULL)
View(data)
head(data)

#mengubah kolom tertentu jadi baris
data <- data.frame(column_to_rownames(data, var = "Name"))

#menghapus kolom tertentu
data <- data[,-12]

#analisis komponwn utama
pca<-prcomp(data,scale=TRUE)
sumpca<-summary(pca)
sumpca

#Plot
#mengelompokkan grup berdasarkan hal tertentu
pch.group <- c(rep(21, times=6), rep(21, times=6), rep(21, times=6))
col.group <- c(rep("skyblue2", times=6), rep("gold", times=6),  
               rep("red2", times=6))

#plot individual
plot(pca$x[,1], pca$x[,2], xlim=c(-4, 4), ylim=c(-3, 3), 
     xlab=paste("PCA 1 (", round(sumpca$importance[2]*100, 1), "%)", sep = ""), 
     ylab=paste("PCA 2 (", round(sumpca$importance[5]*100, 1), "%)", sep = ""), 
     pch=pch.group, col="black", bg=col.group, cex=2, las=1, asp=1)

#grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

#label
text(pca$x[,1], pca$x[,2], labels=row.names(pca$x), pos=c(1,3,4,2), font=2)

#mendapatco-ordinat dari variable dan kalikan dengan 10
l.x <- pca$rotation[,1]*10
l.y <- pca$rotation[,2]*10

#membuat panah
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)

#label posisi
l.pos<-l.y  #membuat vektor dari koordinat sumbu y
lo<-which(l.y<0) #variabel di bawah plot
hi<-which(l.y>0) #variabel di atas

#nilai di vektor
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")

#variable labels
text(l.x, l.y, labels=row.names(pca$rotation), col="red", pos=l.pos)

#legenda
legend("bottomright", legend=c("Center-Back", "Goalkeeper", "Striker"), 
       col="black", pt.bg=c("skyblue2", "gold", "red2"), pch=c(21, 21, 21), 
       pt.cex=1.5)

#mengubah hasil pengamatan ke matrix
tab <- matrix(c(pca$x[,1], pca$x[,2]), ncol=2)

#korelasi
c1 <- cor(tab[1:6,])
c2 <- cor(tab[7:12,])
c3 <- cor(tab[13:18,])

#plot ellpise
polygon(ellipse(c1*(max(abs(pca$rotation))*0.3), centre=colMeans(tab[1:6,]), 
                level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), 
        border="skyblue")
polygon(ellipse(c2*(max(abs(pca$rotation))*0.3), centre=colMeans(tab[7:12,]), 
                level=0.95), col=adjustcolor("gold", alpha.f=0.1), 
        border="gold2")
polygon(ellipse(c3*(max(abs(pca$rotation))*0.3), centre=colMeans(tab[13:18,]), 
                level=0.95), col=adjustcolor("red", alpha.f=0.25), border="red")
