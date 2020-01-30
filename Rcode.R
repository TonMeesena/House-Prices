train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv")
Sale<-train$SalePrice

library("ggplot2")
names(train)
str(train)

ggplot(train,aes(x=SalePrice,y=LotArea))+geom_point()
cor(train$SalePrice, train$LotArea, method = c("pearson", "kendall", "spearman"))

lm1<-lm(SalePrice~ poly(LotArea,1),data=train)
summary(lm1)
plot(train$LotArea,train$SalePrice)
lines(sort(train$LotArea), fitted(lm1)[order(train$LotArea)], col='red', type='b') 

summary(train)

ggplot(data=train, aes(x=SalePrice))+geom_histogram(fill="blue",binwidth = 10000)+
  scale_x_continuous(breaks= seq(0, 800000, by=100000))


ggplot(data=train, aes(x=log(SalePrice)))+geom_histogram(fill="blue",binwidth = 1000000)
