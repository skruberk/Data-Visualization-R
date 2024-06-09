# Random Forest Regression
library(rpart) #for decision tree
library(ggplot2)
library(caTools)
library(randomForest)


# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
set.seed(1234)
regressor <- randomForest(x = dataset[, c("Position", "Level")],
                          y = dataset$Salary,
                          ntree = 500,
                          mtry = 2,      # Number of variables considered for splitting
                          importance = TRUE)#should importance of predictors be assessed

# View dataset
View(dataset)

# Create a data frame for the new data
new_data <- data.frame(Position = "New Position", Level = 6.5)
View(new_data)

# Predict new result
y_pred <- predict(regressor, new_data)
print(y_pred)

# Visualising the Random Forest Regression results (higher resolution)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
# Ensure the grid data frame has the same structure as the training data
grid_data <- data.frame(Position = rep("New Position", length(x_grid)), Level = x_grid)

p <- ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = grid_data)), colour = 'blue') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle('Random Forest Regression') +
  xlab('Level') +
  ylab('Salary')
p
