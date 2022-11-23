library(tidyverse) # metapackage of all tidyverse packages
library(readr)
library(dplyr)
library(magrittr)
library(Metrics)
library(pROC)
library(DMwR)
library(precrec) #for Precision-Recall function
library(ROSE) 
library(nnet) 
library(randomForest)
library(caret)


#You can modify the parameters of the two models if necessary
data = read.csv( "insurance_claims.csv")
str(data)
str(dataset)
summary(data)
data = data[,c(-25,-40)] # delete variables
data = data %>% mutate_if(is.character,as.factor) 
data = as.data.frame(data)
round(prop.table(table(data$fraud_reported)),2)

table(data$fraud_reported)
data$policy_bind_date = as.Date(data$policy_bind_date) - as.Date("1990-01-08")
data$incident_date = as.Date(data$incident_date) - as.Date("2015-01-01") 
train = data.frame(data)
train_index = createDataPartition(train$fraud_reported, times = 1, p=0.7, list=F)
train_data = train[train_index,]
test_data = train[-train_index,]
dim(train_data)
dim(test_data)
set.seed(167)
train_data = ovun.sample(fraud_reported ~.,data=train_data, p=0.5,method="both")$data

# newData <- SMOTE(fraud_reported ~ ., data, perc.over = 100,perc.under=100)
table(train_data$fraud_reported)
model_rf = randomForest(fraud_reported ~.,data=train_data,ntree=500,proximity=TRUE, importance=TRUE)
model_rf
summary(model_rf)
MDSplot(model_rf,train_data$fraud_reported)
print(model_rf)
hist(treesize(model_rf))

table(test_data$fraud_reported)

prediction_rf = predict(model_rf,test_data)
confusionMatrix(prediction_rf,test_data$fraud_reported,positive="Y")

#绘制ROC曲线
ran_roc <- roc(test_data$fraud_reported,as.numeric(prediction_rf))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC curve of random forest model, Mtry =6,ntree=500')

prediction_rf = evalmod(scores = as.numeric(prediction_rf), labels = test_data$fraud_reported, mode = "rocprc")
prediction_rf



# importance of each variable
model_rf$importance
# varImpPlot(model_rf, main = "variable importance")
varImpPlot(model_rf,type=2,pch=15,col=1,cex=1,main="IMPORTANCE(varImpPlot)")




err11=0
err12=0
n_tr=dim(train_data)[1]
n_te=dim(test_data)[1]
for(i in seq(1, 601, 100))
{
  model=nnet(fraud_reported ~ ., data=train_data,maxit=i,size=6,decay = 0.1)
  err11[i]=sum(predict(model,train_data,type='class')!=train_data[,38])/n_tr
  err12[i]=sum(predict(model,test_data,type='class')!=test_data[,38])/n_te
}


error_1 = na.omit(err11)
error_2 = na.omit(err12)
plot(seq(1, 601, 100),error_1,col=1,type="b",ylab="Error rate",xlab="Training epoch",ylim=c(min(min(error_1),min(error_2)),max(max(error_1),max(error_2))))
lines(seq(1, 601, 100),error_2,col=2,type="b")
legend("topleft",pch=c(15,15),legend=c("Train","Test"),col=c(1,2),bty="n")





#Final model and evaluation result
model_best=nnet(fraud_reported ~ ., data=train_data,maxit=300,size=6,decay = 0.1)

prediction_test = predict(model_best,test_data,type="class")

table1 = table(test_data$fraud_reported,prediction_test)
confusionMatrix(table1,positive="Y")


#绘制ROC曲线
# ran_roc <- roc(test_data$fraud_reported,as.numeric(prediction_test))
# plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC curve of random forest model, Mtry =1,ntree=500')

prediction_test = evalmod(scores = as.numeric(prediction_test), labels = test_data$fraud_reported, mode = "rocprc")
prediction_test

library(plyr)
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
  temp <- sample(n,datasize)   #把n打乱
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
  return(cvlist)
}
k <- 10
datasize <- nrow(iris)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1206)
cvlist
###
data <- data
pred <- data.frame()   #存储预测结果
library(plyr)
library(randomForest)
m <- seq(60,500,by = 20)  #如果数据量大尽量间隔大点，间隔过小没有实际意义
for(j in m){   #j指的是随机森林的数量
  progress.bar <- create_progress_bar("text")  #plyr包中的create_progress_bar函数创建一个进度条，
  progress.bar$init(k)   #设置上面的任务数，几折就是几个任务
  for (i in 1:k){
    train <- data[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
    test <- data[cvlist[[i]],]
    model <-randomForest(fraud_reported~.,data = train,ntree = j)   #建模，ntree=j 指的树数
    prediction <- predict(model,subset(test,select = -fraud_reported))   #预测
    randomtree <- rep(j,length(prediction))   #随机森林树的数量
    kcross <- rep(i,length(prediction))   #i是第几次循环交叉，共K次
    temp <- data.frame(cbind(subset(test,select = fraud_reported),prediction,randomtree,kcross))#真实值、预测值、随机森林树数、预测组编号捆绑在一起组成新的数据框tenp
    pred <- rbind(pred,temp)   #temp按行和pred合并
    print(paste("随机森林：",j))  #循环至树数j的随机森林模型
    progress.bar$step() #输出进度条。告知完成了这个任务的百分之几
  }
}

#pred
print("the fraud_reported:")
table(pred$fraud_reported)
print("the prediction:")
table(pred$prediction)
summary(pred)
pred$result = (pred$fraud_reported == pred$prediction)
print("the result of random forest:")
table(pred$result)
table(pred$result)/nrow(pred)


library(plyr)
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
  temp <- sample(n,datasize)   #把n打乱
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
  return(cvlist)
}
k <- 10
datasize <- nrow(iris)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1206)
cvlist
###
data <- data
pred <- data.frame()   #存储预测结果

m <- seq(60,500,by = 20)  #如果数据量大尽量间隔大点，间隔过小没有实际意义
for(j in m){ 
  progress.bar <- create_progress_bar("text")  #plyr包中的create_progress_bar函数创建一个进度条，
  progress.bar$init(k)   #设置上面的任务数，几折就是几个任务
  for (i in 1:k){
    train <- data[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
    test <- data[cvlist[[i]],]
    
    model_best <- nnet(fraud_reported ~ ., data=train,maxit=300,size=6,decay = 0.1)
    prediction <- predict(model,subset(test,select = -fraud_reported),type="class")
    
    kcross <- rep(i,length(prediction))   #i是第几次循环交叉，共K次
    temp <- data.frame(cbind(subset(test,select = fraud_reported),prediction,randomtree,kcross))#真实值、预测值、随机森林树数、预测组编号捆绑在一起组成新的数据框tenp
    pred <- rbind(pred,temp)   #temp按行和pred合并
    print(paste("神经网络：",j))
    progress.bar$step() #输出进度条。告知完成了这个任务的百分之几
  }
}

#pred
print("the fraud_reported:")
table(pred$fraud_reported)
print("the prediction:")
table(pred$prediction)
summary(pred)
pred$result = (pred$fraud_reported == pred$prediction)
print("the result of n:")
table(pred$result)
table(pred$result)/nrow(pred)








dataset = dataset[,c(-25,-40)] # delete variables
dataset = dataset %>% mutate_if(is.character,as.factor) 
dataset = as.data.frame(dataset)
round(prop.table(table(dataset$fraud_reported)),2)

table(dataset$fraud_reported)
dataset$policy_bind_date = as.Date(dataset$policy_bind_date) - as.Date("1990-01-08")
dataset$incident_date = as.Date(dataset$incident_date) - as.Date("2015-01-01") 
newData <- SMOTE(fraud_reported ~ ., dataset, perc.over = 100,perc.under=100)
table(newData$fraud_reported)




############
#   Logistic Regression
#############

# We use the glm function to build a logistic stee regression model

glm.func =glm(factor(isFraud) ~ .:newbalanceOrig,data=train_data, 
              family = binomial
)

















#####################
####Random Forest####
#####################
library(randomForest)
library(caret)

dataset_rf = data.frame(newData)

train = sample(nrow(dataset_rf), 0.7*nrow(dataset_rf), replace = FALSE)
TrainSet = dataset_rf[train,]
TestSet = dataset_rf[-train,]


model_rf = randomForest(fraud_reported ~ ., data=TrainSet,mtry = 75, ntree=500, importance = TRUE)           
model_rf

prediction_rf = predict(model_rf,TestSet)
confusionMatrix(prediction_rf,TestSet$fraud_reported)


#####################
####Neural Network###
#####################

#Calculate the error and find a better model
err11=0
err12=0
n_tr=dim(TrainSet)[1]
n_te=dim(TestSet)[1]
for(i in seq(1, 601, 100))
{
  model=nnet(fraud_reported ~ ., data=TrainSet,maxit=i,size=6,decay = 0.1)
  err11[i]=sum(predict(model,TrainSet,type='class')!=TrainSet[,38])/n_tr
  err12[i]=sum(predict(model,TestSet,type='class')!=TestSet[,38])/n_te
}

error_1 = na.omit(err11)
error_2 = na.omit(err12)
plot(seq(1, 601, 100),error_1,col=1,type="b",ylab="Error rate",xlab="Training epoch",ylim=c(min(min(error_1),min(error_2)),max(max(error_1),max(error_2))))
lines(seq(1, 601, 100),error_2,col=2,type="b")
legend("topleft",pch=c(15,15),legend=c("Train","Test"),col=c(1,2),bty="n")


#Final model and evaluation result
model_best=nnet(fraud_reported ~ ., data=TrainSet,maxit=500,size=6,decay = 0.1)

prediction_test = predict(model_best,TestSet,type="class")
table = table(TestSet$fraud_reported,prediction_test)
confusionMatrix(table)
