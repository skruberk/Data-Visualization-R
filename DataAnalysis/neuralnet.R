#neural net 
library(neuralnet)
library(caTools)
library(ISLR)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(tidymodels)
library(class)

dataset<-iris
any(is.na(dataset))
dataset<-select(iris,-Species)
factordata<-select(iris,-(1:4))
#normalize data
maxs <- apply(dataset, 2, max) #finds max value in the columns, 1 instead for rows
mins <- apply(dataset, 2, min)
datascale<-scale(dataset,center=mins,scale=maxs-mins)
datascale<-as.data.frame(datascale)
fulldata<-cbind(datascale,factordata)
fulldata$Species <- as.numeric(factor(fulldata$Species, levels = c("setosa", "versicolor", "virginica")))


#train test split
set.seed(1234) #make reproducible split 
split<-initial_split(fulldata) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)
n <- names(training_set) #get column names 
# Paste together
f <- as.formula(paste("Species ~", paste(n[!n %in% "Species"], collapse = " + ")))#paste as string for input into nn
#model
nn <- neuralnet(f,data=training_set,hidden = c(5, 3),linear.output=TRUE)


# Compute Predictions off Test Set
pred_val <- predict(nn,test_set[1:4])
str(pred_val)

#convert back from scaled data
# Convert back to non-scaled predictions
predicted_test <- pred_val*(max(fulldata$Species)-min(fulldata$Species))+min(fulldata$Species)
testresults <- (test_set$Species)*(max(fulldata$Species)-min(fulldata$Species))+min(fulldata$Species)
MSE <- sum((testresults - predicted_test)^2)/nrow(test_set)
MSE
error_viz<- data.frame(testresults,predicted_test)
head(error_viz)


p<-ggplot(error_viz,aes(x=testresults,y=predicted_test)) + geom_point() + stat_smooth()+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
