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

ggplot(train,aes(x=factor(OverallQual),y=SalePrice))+geom_boxplot(col='blue')+
  labs(x='Overall Quantity')




ggplot(train,aes(x=GrLivArea,y=SalePrice))+geom_point(col="blue")+
  geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))

NAcol<-which(colSums(is.na(all))>0)

sort(colSums(sapply(all[NAcol],is.na)),decreasing =TRUE)

c<-is.na(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)] <- "None"


all$Fence <- as.character(all$Fence)
all$Fence[is.na(all$Fence)] <- "None"
all$Fence <- as.factor(all$Fence)

Qualitiy<-c("Ex"=3,"Gd"=2,"Fa"=1,"None"=0)


all$PoolQC<-as.integer(revalue(all$PoolQC,Qualitiy))

all[all$PoolQC==0&all$PoolArea>0,c("PoolQC","PoolArea","OverallQual")]

all$PoolQC[2421]<-1
all$PoolQC[2421]<-2
all$PoolQC[2421]<-1
all$PoolQC[2504]<-1
all$PoolQC[2600]<-1


library(dplyr)
all$MiscFeature[is.na(all$MiscFeature)] <- "None"

#temp <- all %>% mutate(MiscFeature = ifelse(is.na(MiscFeature), "None", MiscFeature))
#all<-all %>% mutate(MiscFeature=ifelse(is.na(MiscFeature),"None",MiscFeature))
#all$MiscFeature[is.na(all$MiscFeature)] <- rep("None", )

#temp$MiscFeature<-as.factor(temp$MiscFeature)
ChangeTemp<-c("1"=1,"2"=2,"3"=3,"4"=4,"None"=0)

all$MiscFeature<-as.integer(revalue(all$MiscFeature,ChangeTemp))
#temp<-temp%>%mutate(MiscFeature=ifelse(MiscFeature==5), "0" ,MiscFeature)
#all$MiscFeature<-as.factor(all$MiscFeature)
                                                  


ggplot(all, aes(x=LotArea,y=SalePrice))+geom_point(color="blue")+facet_wrap(~PoolQC)+geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))

ggplot(all,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")

                                              
ggplot(all, aes(x=TotalBsmtSF,y=SalePrice))+geom_point(color="blue")+facet_wrap(~OverallQual)+geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))  



