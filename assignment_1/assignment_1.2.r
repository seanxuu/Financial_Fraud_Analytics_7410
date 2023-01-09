library(tidyverse) # data manipulation 
library(cluster) # clustering algorithms 
library(factoextra) # clustering algorithms & visualization
library(caret) # streamline model training process
library(NbClust)
library(corrplot)
library(gridExtra)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(ggfortify)
dataset = read.csv("creditcard.csv")
str(dataset)
summary(dataset)

# scale amount and time
scale_amount = scale(dataset$Amount)
scale_time = scale(dataset$Time)

data_todo = dataset[-c(1,30,31)] %>% 
  mutate(scale_time)%>% 
  mutate(scale_amount) %>%
  mutate(dataset$Class)
# wss
wssplot <- function(data, nc=15, seed=2345){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(dataset)
# kmeans
kmean <- kmeans(data_todo, 2)
kmean$centers
autoplot(kmean, data_todo,frame = TRUE)
