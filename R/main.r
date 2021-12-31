

install.packages("candisc")
install.packages("dplyr")
library(candisc)
library(dplyr)


my_cols <- c("red", "green", "blue","yellow", "black", "purple","pink") 
fish<-read.csv("FishNum.csv")
data <- fish[ , c(3:7,9)]
category<-unclass(fish[,2])
sex <- fish[,8]
pairs(main="ID : 27020363",data, pch = c(1,4,13)[sex],  cex = 1,
      col = my_cols[unclass(category)],
      lower.panel=NULL)


fishNum<-fish
fish<-read.csv("Fish.csv")


cov(data)
colMeans(fish[,c(3:7,9)])

pca<-prcomp(data,scale=T)
pca
summary(pca)
plot(pca,type="lines",main = "Scree plot(PC vs Eigenvalues)")


lmdata<-lm(cbind(Len1, Len2, Len3, Height, Width,Weight) ~ Species)
lmdata<-candisc(lmdata, term='Species')
cv<-lmdata$coeffs.std
lmdata$coeffs.raw
cv
scores<-lmdata$scores
lmdata
scores



plot(scores[,2],scores[,3],main="ID : 27020363",xlab='canonical variate 1',ylab='canonical variate 2',col=my_cols[fishNum$Species],pch=c(1,4,13)[sex])
plot(scores[,3],scores[,4],main="ID : 27020363",xlab='canonical variate 2',ylab='canonical variate 3',col=my_cols[fishNum$Species],pch=c(1,4,13)[sex])
plot(scores[,4],scores[,5],xlab='canonical variate 3',ylab='canonical variate 4',col=my_cols[fishNum$Species],pch=c(1,4,13)[sex])
plot(scores[,5],scores[,6],xlab='canonical variate 3',ylab='canonical variate 4',col=my_cols[fishNum$Species],pch=c(1,4,13)[sex])



data_and_categories <- fish[ , c(2:7,9)]
attach(data_and_categories)

mean_per_category <- aggregate(data_and_categories , list(Species), FUN=mean) [,c(1,3:8)]
raw_coeff <- lmdata$coeffs.raw
raw_coeff
mean_per_category

