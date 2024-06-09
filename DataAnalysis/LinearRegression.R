library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)
library(ggplot2)
library(ggbeeswarm)
library(viridis)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
# Linear Regression
# Splitting the dataset into the Training set and Test set
set.seed(1353)
split<-initial_split(airquality) #default 3/4 split
train<-training(split)
test<-testing(split)
#visualize the data
p<-ggplot(train, aes(x=Wind, y=Temp)) + geom_point()+geom_smooth(method="lm")+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p 
#model
model<-lm(Temp ~ Wind, data=train)
View(model)
summary(model)
fitted.values(model)
confint(model) #95% confidence interval
View(pairing<-ggpairs(train)) #gives you all the pairs
ggpairs(train)
tidy(model)
augmodel<-augment(model)
view(augmodel)

#check model performance
predict(model, test, interval="confidence") #gives back fitted values, and new observations for the second argument as a data frame
sqrt(mean((test$temp -predict(model, test))^2)) #root mean sq error