# Polynomial Regression

# Importing the dataset
#dataset = read.csv('data.csv')
View(data)
data<-data %>% rename('steps_to_hit'=`Steps to Hit`) 
library(tidyverse)
library(ggplot2)
# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
library(caTools)
set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
#turn dataset into histogram
datahist<-hist(data$steps_to_hit,plot=TRUE)
View(datahist)
write.csv(histresult,"histo_output.csv",row.names = FALSE)
print(histresult)
# Fitting Linear Regression to the dataset
linear_reg <- lm(Salary~Level,
             data = data)
summary(linear_reg)
# Fitting Polynomial Regression to the dataset
data$poly2=data$steps_to_hit^2

poly2<-lm(Salary~ Level+ I(Level^2), data=data)
summary(poly2)
poly3<-lm(Salary~ Level+ I(Level^2)+ I(Level^3), data=data) #have to include all the terms 
summary(poly3)
poly4<-lm(Salary~ Level+ I(Level^2)+ I(Level^3)+ I(Level^4), data=data) #have to include all the terms 
summary(poly4)
p+  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
#compare linear and polynomial regression
p<-ggplot() +
  scale_color_viridis(discrete=TRUE, option = "H")+
  geom_point(aes(x = data$Level, y = data$Salary))+
  geom_line(aes(x = data$Level, y = predict(linear_reg, newdata = data),color="linear_reg"))+
  geom_line(aes(x = data$Level, y = predict(poly2, newdata = data), color="poly2"))+
  geom_line(aes(x = data$Level, y = predict(poly3, newdata = data),color="poly3"))+
  geom_line(aes(x = data$Level, y = predict(poly4, newdata = data),color="poly4"))+
  ggtitle('Linear vs Polynomial Regression') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Level') +
  ylab('Salary')
p


#
