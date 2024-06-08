# Decision Tree Regression
library(rpart) #for decision tree
library(ggplot2)
library(caTools)
# Importing the dataset
dataset = read.csv('data.csv')

# could split the dataset into the Training set and Test set
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Decision Tree Regression to the dataset
regressor <- rpart(formula = Salary ~ Level,
                  data = data,
                  control = rpart.control(minsplit = 1))
# Create a data frame for the new data
new_data <- data.frame(Level = 6.5)#predicting for 6.5
# Predict new result
y_pred = predict(regressor, new_data )
print(y_pred)
# Visualising the Decision Tree Regression results (higher resolution)
x_grid = seq(min(datas$Level), max(data$Level), 0.1)
p<-ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggtitle('decision tree regression') +
  xlab('Level') +
  ylab('Salary')
p

# Plotting the tree
plot(regressor)
text(regressor)