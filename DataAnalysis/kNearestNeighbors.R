# K-Nearest Neighbors (K-NN)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
library(class)
library(ElemStatLearn)
# Importing the dataset
dataset = dataset[3:5]
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
View(dataset)
# Splitting the dataset into the Training set and Test set
set.seed(1353)
split<-initial_split(dataset) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)

# Feature Scaling
train_scaled <- scale(training_set[, -3])  # Scale only numerical features in the training set
test_scaled <- scale(test_set[, -3])    # Scale only numerical features in the test set
View(training_set)
View(test_set)
# Combine scaled features with non-scaled target variable
training_set <- cbind(train_scaled, Purchased = training_set$Purchased)
test_set <- cbind(test_scaled, Purchased = test_set$Purchased)

# Fitting K-NN, predict test set
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5,
             prob = TRUE)

# Confusion Matrix
cm = table(test_set[, 3], y_pred)
View(cm)
# training set

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-Nearest Neighbors (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# test set
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))