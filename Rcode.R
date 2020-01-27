train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv")
Sale<-train$SalePrice

library("ggplot2")
names(train)
str(train)

ggplot(train,aes(x=LotArea,y=SalePrice))+geom_point()+geom_smooth()
cor(train$SalePrice, train$LotArea, method = c("pearson", "kendall", "spearman"))
