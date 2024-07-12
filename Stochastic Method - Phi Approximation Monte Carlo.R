numtrials<-1000
points<-data.frame(x=runif(numtrials,-0.5,0.5),
                   y=runif(numtrials,-0.5,0.5))
head(points)



library(dplyr)
points<-points%>%mutate(radius=sqrt(x^2+y^2),
                        pointincircle=ifelse(radius<=0.5, TRUE, FALSE))
library(ggplot2)
ggplot(points)+ geom_point(aes(x=x, y=y, color=pointincircle), 
                          size=0.7) +theme_minimal()

ratio<-sum(points$pointincircle)/nrow(points)
piapprox<-4*ratio
piapprox