train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv")
Sale<-train$SalePrice
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
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

test$SalePrice<-NA

train$Id<-NULL
test$Id<-NULL
all=rbind(train,test)

ggplot(data=train, aes(x=SalePrice))+geom_histogram(fill="blue",binwidth = 10000)+
  scale_x_continuous(breaks= seq(0, 800000, by=100000))

numericVars<-which(sapply(all,is.numeric))


numericVarNames<-names(numericVars)

all_numVar<-all[,numericVars]

cor_numVar<-cor(all_numVar,use="pairwise.complete.obs")



cor_sorted<-as.matrix(sort(cor_numVar[,'SalePrice'],decreasing=TRUE))


CorHigh<-names(which(cort_sorted,1,function(x) abs(x)>0.5))
cor_numVar<-cor_numVar[CorHigh,CorHigh]

corrplot.mixed(cor_numVar,tl.col="black",tl.pos="lt")

ggplot(train,aes(x=factor(OverallQual),y=SalePrice))+geom_boxplot(col='blue')+labs(x='Overall Quantity')




ggplot(train,aes(x=GrLivArea,y=SalePrice))+geom_point(col="blue")+geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))

NAcol<-which(colSums(is.na(all))>0)

sort(colSums(sapply(all[NAcol],is.na)),decreasing =TRUE)

c<-is.na(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)]<-as.factor('a')
all$PoolQC[is.na(all$PoolQC)] <- 'None'
replace(is.na(all$PoolQC),"None")
                                                                                                                                                     clear(numer)
