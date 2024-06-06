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
# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

View(pairing<-ggpairs(springbig)) #gives you all the pairs
ggpairs(data)
#multiple linear regression

summary(s_model)
plot(s_model)
fitted<-predict(s_model,springbig)
springbig$pred<-fitted
View(springbig)
tidy(s_model)
View(augment(s_model))#adds predictions cluster assignments, residuals
View(modelglance<-glance(s_model))

multmodel<- lm(exponent ~ discs+radius+spring+beta1+beta3, data = springbig) #use a '.' to just list all the variables except the one left of ~
summary(multmodel)
multmodel<- lm(exponent ~ discs+radius+spring+beta3, data = springbig) #use a '.' to just list all the variables except the one left of ~
summary(multmodel)
multmodel<- lm(exponent ~ discs+radius+beta3, data = springbig) #use a '.' to just list all the variables except the one left of ~
summary(multmodel)
multmodel<- lm(exponent ~ discs+beta3, data = springbig) #use a '.' to just list all the variables except the one left of ~
summary(multmodel)
#t values over one accepted regardless of p value (Pr(>|t|))
#plot the residuals 
View(performancespring<-augment(multmodel))
p<-ggplot(performancespring, aes(x=.fitted,y=.resid)) +geom_point() + geom_hline(yintercept=0)
p
#multiple linear regression
s_model <- lm(exponent ~ discs+radius, data = springbig)
summary(s_model)
plot(s_model)
fitted<-predict(s_model,springbig)
springbig$pred<-fitted
View(springbig)
tidy(s_model)
View(augment(s_model))#adds predictions cluster assignments, residuals
View(modelglance<-glance(s_model))
# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)