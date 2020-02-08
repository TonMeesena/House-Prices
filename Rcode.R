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


ggplot(all, aes(x=MoSold,y=SalePrice ))+facet_wrap(~YrSold)+geom_hline(yintercept = 196000, linetype="dashed",color="red")+geom_bar(stat="summary",fill="blue",fun.y="mean")

all$MoSold<-as.factor(all$MoSold)






ggplot(all[!is.na(all$SalePrice),],aes(x=MoSold,y=SalePrice))+geom_bar(stat="summary",fun.y="median",fill="blue")+facet_wrap(~YrSold)

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

tree_train2<-randomForest(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd
,train_train,ntree=500,mtry=15,importance=TRUE)

plot(tree_train2)

valid_high_pred<-predict(tree_train2,train_valid)

hist(valid_high_pred)
hist(train_valid$SalePrice)
rmse(valid_high_pred,train_valid$SalePrice)


#neural network 
neural_train<-neuralnet(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd
,hidden=c(3,1),data=train_train,linear.output=T,stepmax=1e+07)

valid_nn<-predict(neural_train,train_valid)
RMSE(valid_nn,train_valid$SalePrice)
hist(valid_nn)



#regression
fit<-lm(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd,data=train_train)

valid_pred<-predict(fit,train_valid)
RMSE(valid_pred,train_valid$SalePrice)






