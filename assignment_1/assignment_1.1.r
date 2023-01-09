# Install packages/load libraries
library(corrplot)
library(caret)
library(tidyverse)
library(cowplot)
# Import the data
dataset = read.csv("PS_20174392719_1491204439457_log.csv")
head(dataset,10)
#Summarize each variable in the dataset
summary(dataset)
#display rows and columns
dim(dataset)
str(dataset)
# Let the ggplot be an object.
data_ggplot = ggplot(data = dataset)
data_ggplot + geom_bar(mapping = aes(x = step,fill = step))+
  labs(title = 'Distribution of step')
# step%%24 
step_24hrs=dataset$step %%24
data_ggplot + geom_bar(mapping = aes(x = step_24hrs,fill=step_24hrs))+
  labs(x="Time",y="Frequency",title = "Transactions during a whole day")
# type
data_ggplot + geom_bar(mapping = aes(x = type,fill=type))+
  labs(title = 'The count of different types')
# amount < 200000
hist(dataset$amount[dataset$amount<2000000],
     main = 'Histogram of Amount < 2000000',
     freq = T,
     col = 'black',
     density = 10,
     xlab = 'amount',
     ylim=c(0,4000000),
    ylab="count",)

# 4 features 
plot1 = data_ggplot + geom_histogram(mapping = aes(x=oldbalanceOrg  ))
plot2 = data_ggplot + geom_histogram(mapping = aes(x=newbalanceOrig))
plot3 = data_ggplot + geom_histogram(mapping = aes(x=oldbalanceDest  ))
plot4 = data_ggplot + geom_histogram(mapping = aes(x=newbalanceDest  ))
plot_grid(plot1, plot2,plot3,plot4 ,labels = c("oldbalanceOrg", "newbalanceOrig","newbalanceOrig",'newbalanceDest'))


fac1 = factor(dataset$isFraud)
# isFraud class vs Hour of the day ('isFraud' vs 'step%%24')
dataset$step_24hrs=dataset$step %%24
ggplot(dataset,aes(x=step_24hrs,fill=factor(isFraud))) +
  geom_histogram(bins=24)+
  labs( x = 'Hours',y = 'Transactions')+
  ggtitle('Distribution of time in a day of Non-Fraud & Fraud Transaction')+
  scale_fill_discrete(name = "Non Fraud & Fraud")+
  facet_wrap(.~isFraud, scales = 'free')+
  theme(legend.position="top")


# non fraud and fraud 
isNonFraud_type_set = subset(dataset,dataset$isFraud == 0)
isFraud_type_set = subset(dataset,dataset$isFraud == 1)

ggplot(data = isNonFraud_type_set) + geom_jitter(mapping = aes(x = type,y =amount, fill=type))+
  labs(title = 'The count of different types in NonFraud_type_set ')
ggplot(data = isFraud_type_set) + geom_jitter(mapping = aes(x = type,y =amount, fill=type))+
  labs(title = 'The count of different types in Fraud_type_set')

# Distribution of transaction amount by Non Fraud & Fraud Transaction
isfraud_amount_type <- subset(dataset,dataset$type == 'CASH_OUT'|dataset$type == 'TRANSFER')
ggplot(isfraud_amount_type,aes(x=amount,y=isFraud,colour=type)) +
  geom_point()+
  labs( x = 'Amount of transaction',y = 'Non Fraud & Fraud Transaction')+
  ggtitle('Distribution of transaction amount by Non Fraud & Fraud Transaction')


# type with isFlaggedFraud ==1
dataset_sub = subset(dataset,dataset$isFlaggedFraud == 1)
ggplot(data = dataset_sub) + 
  geom_bar(mapping = aes(x = type,fill=type))+
  labs(title = 'The count of different types')



#A CORRELATION MATRIX IS CREATED
corr <- sapply(dataset, is.numeric)
corrchart <- cor(dataset[,corr])
corrplot(corrchart, main = '\n\n Correlation Chart',method = "number")
