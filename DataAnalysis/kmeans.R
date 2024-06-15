#k means clustering 
library(ggplot2)
library(caTools)
library(randomForest)
library(rpart.plot)
library(caret)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
library(class)
library(ElemStatLearn)
library(e1071)
library(cluster)

p<-ggplot(dataset,aes(circ,solidity,color=group)) +geom_point()+
  theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle('Initial Data Vis') 
p
set.seed(1234)

cluster <- kmeans(dataset[, 2:3], 4, nstart = 20)
cluster
table(cluster$cluster,dataset$group)
p<-clusplot(dataset,cluster$cluster,color=T,shade=T,lines=0,labels=4,
            main = "Cluster Plot with K-means Clustering")
p


