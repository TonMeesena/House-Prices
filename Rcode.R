train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv")
Sale<-train$SalePrice

install.packages("corrplot")
install.packages("knitr")
install.packages("randomForest")
install.packages("dplyr")
install.packages("Metrics")
install.packages("neuralnet")
install.packages("caret")
install.packages("tidyverse")
library(tidyverse)
library(caret)
library(neuralnet)
library(Metrics)
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
library(tidyverse)
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
                                                  


ggplot(all, aes(x=LotArea,y=SalePrice))+geom_point(color="blue")+facet_wrap(~PoolQC)+
  geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))

ggplot(all,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")

                                              
ggplot(all, aes(x=TotalBsmtSF,y=SalePrice))+geom_point(color="blue")+facet_wrap(~OverallQual)+
  geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))  


ggplot(all, aes(x=MoSold,y=SalePrice ))+facet_wrap(~YrSold)+geom_hline(yintercept = 196000, linetype="dashed",color="red")+
  geom_bar(stat="summary",fill="blue",fun.y="mean")

all$MoSold<-as.factor(all$MoSold)






ggplot(all[!is.na(all$SalePrice),],aes(x=MoSold,y=SalePrice))+geom_bar(stat="summary",fun.y="median",fill="blue")+
  facet_wrap(~YrSold)

CorHigh<-names(which(apply(cor_sorted,1,function(x) abs(x)>0.5)))
cor_numvarH<-cor_numVar[CorHigh,CorHigh]

corrplot.mixed(cor_numvarH,tl.co="black",tl.pos="lt",tl.cex=0.7,cl.cex=0.7,number.cex=0.7)







set.seed(1)
train_train<-sample(nrow(train),0.7*nrow(train),replace=FALSE)


TrainSet<-train[train_train,]
ValidSet<-train[-train_train,]

summary(TrainSet)



model1<-randomForest(SalePrice~.,data=TrainSet,importance=TRUE)


data1 <- read.csv(file.choose(), header = TRUE)
names(data1)<-c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
set.seed(7)
trainNum<-sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
TrainSetCar<-data1[trainNum,]
ValidSetCar<-data1[-trainNum,]
model1 <- randomForest(Condition ~ ., data = TrainSetCar, importance = TRUE)

model2<-randomForest(Condition ~.,data=TrainSetCar,ntree=500,mtry=6,importance=TRUE)

predTrain<-predict(model2,TrainSetCar,type="class")


table(predTrain, TrainSetCar$Condition)

predValid<-predict(model2,ValidSetCar,type="class")
mean(predValid==ValidSetCar$Condition)
table(predValid,ValidSetCar$Condition)


importance(model2)
varImpPlot(model2)



set.seed(1)
num<-sample(nrow(test), 0.7*nrow(test))

train_train<-train[num,]
train_valid<-train[-num,]

tree_train<-randomForest(SalePrice~ ,train_train,)


sapply(train,function(y) sum(length(which(is.na(y)))))

name<-names(train[,train %>% apply(2,function(x) all(!is.na(x)))])

sapply(train[,name], function(y) sum(length(which(is.na(y)))))

formu<-paste(name,sep="",collapse="+")

a <- train_train %>% 
  select_if(~sum(is.na(.)) == 0) 



tree_train<-randomForest(SalePrice~. - SalePrice,a,ntree=500,mtry=15,importance=TRUE)
plot(tree_train)

b<-train_valid %>%
  select_if(~sum(is.na(.))==0)
valid_pred<-predict(tree_train,b)

RMSE(valid_pred,train_valid$SalePrice)
hist(valid_pred,breaks=100)
hist(train_valid$SalePrice,breaks=100)
c<-valid_pred - train_valid$SalePrice
hist(c,breaks=100)



#random Forest for high correlation 
for2<-paste(CorHigh,collapse="+")

tree_train2<-randomForest(SalePrice~OverallQual+GrLivArea+
                            GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+
                            YearBuilt+YearRemodAdd,train_train,ntree=500,mtry=15,importance=TRUE)

plot(tree_train2)

valid_high_pred<-predict(tree_train2,train_valid)

hist(valid_high_pred)
hist(train_valid$SalePrice)
RMSE(valid_high_pred,train_valid$SalePrice)


#neural network 
neural_train<-neuralnet(SalePrice~OverallQual+GrLivArea+GarageCars+
                          GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+
                          YearRemodAdd, hidden=c(3,1),data=train_train,linear.output=T,stepmax=1e+07)

valid_nn<-predict(neural_train,train_valid)
RMSE(valid_nn,train_valid$SalePrice)
hist(valid_nn)
plot(valid_nn)


#regression
fit<-lm(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+
          X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd,data=train_train)

valid_pred<-predict(fit,train_valid)
RMSE(valid_pred,train_valid$SalePrice)

train_withID <- train_train %>%
  rownames_to_column("ID") %>%
  select(c("ID", "SalePrice"))
train_valid_withID <- train_valid %>%
  rownames_to_column("ID") %>%
  select(c("ID", "SalePrice"))



data_for_plot <- left_join(train_withID, train_valid_withID)


#visualize
ggplot(train_train,aes(x=GarageCars,y=SalePrice))+geom_point(color="blue")
ggplot(train_valid,aes(x=GarageCars,y=valid_pred))+geom_point(color="red")

ggplot() +
  geom_point(data = train_valid_withID, aes(x = YearRemodAdd, y = SalePrice), color = "blue") +
  geom_point(data = train_valid_withID, aes(x = YearRemodAdd, y = valid_pred), color = "red")


ggplot(data=train,aes(x=MoSold,y=SalePrice))+geom_bar(fill="green",stat="summary",fun.y="median")+facet_wrap(~YrSold)+
  geom_hline(yintercept = 180000)

#numdate
train2<-train
numDate<-train2$MoSold+(train2$YrSold-2005)*12
train2<-cbind(train2,numDate)
train2$SalePrice<-log(train2$SalePrice)
hist(train2$SalePrice)
ggplot(data=train2,aes(x=numDate,y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y="median")

#Check each feature
#MSZoning
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+facet_grid(~MSZoning )

#LotFrontage
ggplot(data=train,aes(x=LotFrontage,y=SalePrice))+geom_point(color="blue")

#LotArea
ggplot(data=train,aes(x=LotArea,y=SalePrice))+geom_point(color="blue")

#Street
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+facet_wrap(~LotShape)
ggplot(data=train,aes(x=OverallQual,fill=SalePrice,color=LotShape))+geom_bar(summary="stat",fun.y="mean")

#LandContour
ggplot(data=train,aes(x=LandContour,fill=SalePrice))+geom_bar(fill="blue",summary="stat",fun.y="mean")
ggplot(data=train,aes(x=LandContour,y=SalePrice))+geom_point(color="blue")
ggplot(data=train,aes(x=OverallQual,y=SalePrice,color=LandContour))+geom_point()

#Utilities
ggplot(data=train,aes(x=OverallQual,y=SalePrice,color=Utilities))+geom_point()

#LotConfig
ggplot(data=train,aes(x=OverallQual,y=SalePrice,color=LotConfig))+geom_point()
ggplot(data=train,aes(x=OverallQual,y=SalePrice,fill=LotConfig))+geom_bar(stat="summary",fun.y="mean")
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="green")+facet_wrap(~LotConfig)
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="orange")+facet_wrap(~LotConfig)
ggplot(data=train,aes(x=LotConfig,y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="red")

#LandSlope
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="green")+facet_wrap(~LandSlope)
ggplot(data=train,aes(x=LandSlope,y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="red")
ggplot(data=train,aes(x=OverallQual,y=SalePrice,color=LandSlope))+geom_point()

#Neighborhood
ggplot(data=train,aes(x=OverallQual,y=SalePrice,color=Neighborhood))+geom_point()

ggplot(data=train,aes(x=Neighborhood,y=SalePrice))+geom_bar(stat="summary",fun.y="median",fill="red")

train_by_Neighbor <- train %>% 
  group_by(Neighborhood) %>% 
  dplyr::summarise(MeanSales = mean(SalePrice))

a <- train %>% 
  group_by(Neighborhood) %>% 
  filter(SalePrice > mean(SalePrice))

ggplot(a,aes(x=Neighborhood,y=SalePrice))+geom_point(fill="blue")
ggplot(train,aes(x=Neighborhood,y=SalePrice))+geom_point(alpha=0.2,color="blue")+geom_point(color="red",data=a)


plot(MeanSales~Neighborhood,data=train_by_Neighbor)
ggplot(data=train_by_Neighbor,aes(x=Neighborhood, y=MeanSales))+geom_point(fill="blue")
ggplot(data=train_by_Neighbor,aes(x=Neighborhood, weight=MeanSales))+geom_bar(fill="blue") 

train_by_Neighbor_g <-train_by_Neighbor %>% 
  filter(MeanSales>median(MeanSales))
train_by_Neighbor_l <-train_by_Neighbor %>% 
  filter(MeanSales<median(MeanSales))

ggplot(data=train_by_Neighbor_g,aes(x=Neighborhood, weight=MeanSales))+geom_bar(fill="blue") 
  
  
ggplot(data=train_by_Neighbor,aes(x=Neighborhood, weight=MeanSales))+geom_bar(data=train_by_Neighbor_g,fill="blue") +
  geom_bar(data=train_by_Neighbor_l,fill="red")



#Condition1
ggplot(data=train,aes(x=Condition1, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=Condition1, y=SalePrice))+geom_point(color="blue")

#Condition2
ggplot(data=train,aes(x=Condition2, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=Condition2, y=SalePrice))+geom_point(color="blue")


#BldgType
ggplot(data=train,aes(x=BldgType, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=BldgType, y=SalePrice))+geom_point(color="blue")

#HouseStyle
ggplot(data=train,aes(x=HouseStyle, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=HouseStyle, y=SalePrice))+geom_point(color="blue")

#OverallQual
ggplot(data=train,aes(x=OverallQual, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=OverallQual, y=SalePrice))+geom_point(color="blue")

#OverallCond
ggplot(data=train,aes(x=OverallCond, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=OverallCond, y=SalePrice))+geom_point(color="blue")

#YearBuilt
ggplot(data=train,aes(x=YearBuilt, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 
ggplot(data=train,aes(x=YearBuilt, y=SalePrice))+geom_point(color="blue")


ggplot(train, aes(x = as.factor(YearBuilt), y = SalePrice,color=OverallQual)) + 
  geom_point(size=2) + 
  stat_summary(fun.y=median, aes(group=1), geom="line", colour="blue") 


#YearRemodAdd
ggplot(data=train,aes(x=YearRemodAdd, y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean) 

ggplot(train, aes(x = YearRemodAdd, y = SalePrice,color=OverallQual)) + 
  geom_point(size=2) + 
  stat_summary(fun.y=median, aes(group=1), geom="line", colour="blue") 

#RoofStyle

ggplot(data=train,aes(x=RoofStyle,y=SalePrice))+geom_point(color="blue")+
  stat_summary(fun.y=median,aes(group=1),geom="line",color="red")
ggplot(data=train,aes(x=RoofStyle,y=SalePrice))+geom_bar(fill="blue",stat="summary",fun.y=mean)


#RoofMatl

ggplot(data=train,aes(x=RoofMatl,y=SalePrice))+geom_point(color="blue")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

#Exterior1st
ggplot(data=train,aes(x=Exterior1st,y=SalePrice))+geom_point(color="blue")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

ggplot(data=train,aes(x=Exterior1st,y=SalePrice))+
  geom_bar(fill="blue",stat="summary",fun.y=median)+
  geom_hline(yintercept=median(train$SalePrice))




#Exterior2nd
ggplot(data=train,aes(x=Exterior2nd,y=SalePrice))+geom_point(color="blue")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

ggplot(data=train,aes(x=Exterior2nd,y=SalePrice))+
  geom_bar(fill="blue",stat="summary",fun.y=median)+
  geom_hline(yintercept=median(train$SalePrice))


#MasVnrType
ggplot(data=train,aes(x=MasVnrType,y=SalePrice))+geom_point(color="blue")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

ggplot(data=train,aes(x=MasVnrType,y=SalePrice))+
  geom_bar(fill="blue",stat="summary",fun.y=median)+
  geom_hline(yintercept=median(train$SalePrice))


#MasVnrArea
ggplot(data=train,aes(x=MasVnrArea,y=SalePrice))+geom_point(alpha=0.3,color="blue")+
  geom_smooth(method="lm",color="red")+geom_point(color="red",size=4,data=train[which(is.na(train$MasVnrArea)),])



#ExterQual

ggplot(data=train,aes(x=ExterQual,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

ggplot(data=train,aes(x=ExterQual,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")

ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+facet_wrap(~ExterQual)
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

#Foundation

ggplot(data=train,aes(x=Foundation,y=SalePrice,color=OverallQual))+geom_point()+
    stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
  
  
ggplot(data=train,aes(x=Foundation,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")
  
  
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+facet_wrap(~Foundation)
stat_summary(fun.y=median,color="red",geom="line",aes(group=1))
  
  
  
#BsmtQual

ggplot(data=train,aes(x=BsmtQual,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
ggplot(data=train,aes(x=BsmtQual,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")
  
  
ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~BsmtQual)+geom_smooth(method="lm")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))
  

#BsmtCond

ggplot(data=train,aes(x=BsmtCond,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
ggplot(data=train,aes(x=BsmtCond,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")


ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~BsmtCond)+geom_smooth(method="lm")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

#BsmtExposure
ggplot(data=train,aes(x=BsmtExposure,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
ggplot(data=train,aes(x=BsmtExposure,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")


ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~BsmtExposure)+geom_smooth(method="lm")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

#BsmtFinType1

ggplot(data=train,aes(x=BsmtFinType1,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
ggplot(data=train,aes(x=BsmtFinType1,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")


ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~BsmtFinType1)+geom_smooth(method="lm")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))

#BsmtFinSF1

ggplot(data=train,aes(x=BsmtFinSF1,y=SalePrice,color=OverallQual))+geom_point()


ggplot(data=train,aes(x=BsmtFinSF1,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~OverallQual)+geom_smooth(method="lm")


#TotalBsmtSF

ggplot(data=train,aes(x=TotalBsmtSF,y=SalePrice,color=OverallQual))+geom_point()


ggplot(data=train,aes(x=TotalBsmtSF,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~OverallQual)+geom_smooth(method="lm")



#Heating

ggplot(data=train,aes(x=Heating,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))+geom_boxplot(alpha=0.1)
ggplot(data=train,aes(x=Heating,y=SalePrice))+geom_bar(stat="summary",fun.y=median,fill="red")


ggplot(data=train,aes(x=OverallQual,y=SalePrice))+geom_point(color="blue")+
  facet_wrap(~Heating)+geom_smooth(method="lm")+
  stat_summary(fun.y=median,color="red",geom="line",aes(group=1))


#HeatingQC
ggplot(data=train,aes(x=HeatingQC,y=SalePrice,color=OverallQual))+geom_point()+
  stat_summary(fun.y=median, color="red",geom="line",aes(group=1))+
  geom_boxplot(alpha=0.2)
ggplot(data=train,aes(x=OverallQual,y=SalePrice,fill=HeatingQC))+geom_bar(stat="summary",fun.y=median)


#Add Age Reno01
train$Age<-train$YrSold-train$YearRemodAdd
train$Reno01<-ifelse(train$YearRemodAdd==train$YearBuilt,0,1)
ggplot(train,aes(x=Age,y=SalePrice,color=Reno01))+geom_point()+geom_smooth(method="lm",color="red")

ggplot(train,aes(x=as.factor(Reno01),y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="blue")+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6)


#Add IsNew
train$IsNew<-ifelse(train$YrSold==train$YearBuilt,1,0)
ggplot(train,aes(x=as.factor(IsNew),y=SalePrice))+geom_bar(stat="summary",fun.y="mean",fill="blue")+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6)+
  geom_boxplot(alpha=0.1)

ggplot(train,aes(x=OverallQual,y=SalePrice))+geom_point()+
  geom_smooth(data= train[which(train$IsNew==0),],method="lm",color="red")+
  geom_smooth(data= train[which(train$IsNew==1),],method="lm",color="gold")+
  geom_point(data=train[train$IsNew==1,],size=2,color="orange")


#Neighborhood

ggplot(data=train,aes(x=reorder(Neighborhood, SalePrice,FUN=median),y=SalePrice))+
  geom_bar(stat="summary",fun.y =median,fill="blue")+
  geom_label(stat="count",aes(label=..count..,y=..count..))+
  geom_hline(yintercept=median(train$SalePrice),color="red",linetype="dashed")+
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(y="Median SalePrice")


ggplot(data=train,aes(x=reorder(Neighborhood, SalePrice,FUN=mean),y=SalePrice))+
  geom_bar(stat="summary",fun.y =mean,fill="blue")+
  geom_label(stat="count",aes(label=..count..,y=..count..))+
  geom_hline(yintercept=mean(train$SalePrice),color="red",linetype="dashed")+
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(y="Mean SalePrice")




ggplot(data=train,aes(x=reorder(Neighborhood,SalePrice,FUN=mean),y=SalePrice,color=OverallQual))+
  geom_point(alpha=0.5)+stat_summary(fun.y=mean,geom="line",aes(group=1))+
  stat_summary(fun.y=min,geom="line",aes(group=1))+
  stat_summary(fun.y=max,geom="line",aes(group=1))+
  geom_point(data=train[which(as.numeric(train$OverallQual)>9),],color="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


train$Neighborhood<-as.factor(train$Neighborhood)
train$NeighRich[train$Neighborhood %in% c("StoneBr","NoRidge","NridgHt")]<-2
train$NeighRich[train$Neighborhood %in% c("MeadowV","IDOTRR","BrDale")]<-0
train$NeighRich[is.na(train$NeighRich)]<-1


#TotalArea

ggplot(data=train,aes(x=GrLivArea,y=SalePrice))+geom_point(color="blue")+
  geom_smooth(method="lm",color="red")

ggplot(data=train,aes(x=TotalBsmtSF,y=SalePrice))+geom_point(color="blue")+
  geom_smooth(method="lm",color="red")

train$TotalArea<-train$GrLivArea+train$TotalBsmtSF


ggplot(data=train,aes(x=TotalArea,y=SalePrice))+geom_point(color="blue")+
  geom_smooth(method="lm",color="red")


cor(train$SalePrice,train$TotalArea,use="pairwise.complete.obs")
cor(train$SalePrice,train$GrLivArea,use="pairwise.complete.obs")
cor(train$SalePrice,train$TotalBsmtSF,use="pairwise.complete.obs")




#random Forest 2
set.seed(2)
num<-sample(nrow(test), 0.7*nrow(test))

train_train2<-train[num,]
train_valid2<-train[-num,]

tree_train22<-randomForest(SalePrice~OverallQual+TotalArea+NeighRich+IsNew+Age+FullBath
                           ,train_train2,ntree=1000,mtry=8,importance=TRUE)

plot(tree_train22)

valid_high_pred2<-predict(tree_train22,train_valid2)

hist(valid_high_pred2)
hist(train_valid2$SalePrice)
RMSE(valid_high_pred2,train_valid2$SalePrice)

#regression 2

regress_train<-lm(SalePrice~OverallQual+TotalArea+NeighRich+IsNew+Age,train_train2)

train_regress_pred<-predict(regress_train,train_valid2)

RMSE(train_regress_pred,train_valid2$SalePrice)


plot(train_valid2$SalePrice)+plot(valid_high_pred2)


ggplot()+geom_point(data=train_valid,aes(x=row.names(train_valid),y=valid_high_pred2),color="blue")+
  geom_point(data=train_valid,aes(x=row.names(train_valid),y=SalePrice),color="red")

# Check error for regression
ggplot(data=train_valid2)+
  geom_bar(aes(x=reorder(row.names(train_valid),abs(valid_high_pred2-SalePrice),FUN=identity),weight=abs(valid_high_pred2-SalePrice),color=OverallQual))

ggplot(data=train_valid2[,])+
  geom_bar(aes(x=reorder(row.names(train_valid),abs(valid_high_pred2-SalePrice)/SalePrice*100,FUN=identity),weight=abs(valid_high_pred2-SalePrice)/SalePrice*100,color=OverallQual))

ggplot(data=train_valid2[,])+
  geom_bar(aes(x=reorder(row.names(train_valid),OverallQual,FUN=identity),weight=abs(valid_high_pred2-SalePrice)/SalePrice*100,color=OverallQual))


#
ggplot(data=train_valid2[,])+
  geom_bar(aes(x=reorder(row.names(train_valid),OverallQual,FUN=identity),weight=abs(train_regress_pred-SalePrice)/SalePrice*100,color=OverallQual))


#Prepare to submit
testsub<-test

#Fill

testsub$Age<-testsub$YrSold-testsub$YearRemodAdd

testsub$IsNew<-ifelse(testsub$YrSold==testsub$YearBuilt,1,0)


testsub$NeighRich[testsub$Neighborhood %in% c("StoneBr","NoRidge","NridgHt")]<-2
testsub$NeighRich[testsub$Neighborhood %in% c("MeadowV","IDOTRR","BrDale")]<-0
testsub$NeighRich[is.na(testsub$NeighRich)]<-1


testsub$TotalArea<-testsub$GrLivArea+testsub$TotalBsmtSF



testsub_fr<-randomForest(SalePrice~OverallQual+TotalArea+NeighRich+IsNew+Age
                           ,train,ntree=1000,mtry=5)

testsub[661,"TotalArea"]<-testsub[661,"GrLivArea"]
dd<-predict(testsub_fr,testsub)

testsub_final<-data.frame("Id"=testsub$Id,"SalePrice"=dd)

write_csv(testsub_final,"submission")

train3<-train


#Test

NumVar<-which(sapply(train,is.numeric))
NameNumVar<-names(train[,NumVar])


for(i in 1:length(NumVar)){
  if(abs(skew(train3[,NumVar[i]]))>0.8){
    
    train3[,NumVar[i]]<-log(train3[,NumVar[i]]+1)
    
  }
}


#Eliminate NA












