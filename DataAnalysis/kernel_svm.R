# Kernel SVM if data are not linearly separable
#RBF kernel takes two two data points as input (x1 and x2) and transforms them 
#into a higher-dimensional feature space: K(x1, x2) = exp(-gamma * ||x1 - x2||^2)
# K(x1, x2) similarity btw data points x1 and x2.
# #||#Euclidean distance between the two data points.
#gamma is the weight and important performance metric to choose
ibrary(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
library(class)
library(ElemStatLearn)
library(e1071)
# Importing the dataset
dataset = dataset[3:5]
View(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
set.seed(123)
split<-initial_split(dataset) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)
View(training_set)
# Feature Scaling
training_set_numeric <- training_set[, -3]
test_set_numeric <- test_set[, -3]
train_scaled <- scale(training_set_numeric)
test_scaled <- scale(test_set_numeric)
View(train_scaled)
train_purchase<-training_set[ , -(1:2)]
test_purchase<-test_set[ , -(1:2)]
# Recombine with target variable df_
training_set <- cbind(train_scaled,train_purchase)
View(training_set)
test_set <- cbind(test_scaled,test_purchase)

# Fitting Kernel SVM to the Training set
classifier <- svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification', #eps-regression, nu-regression
                 kernel = 'radial') #could also do sigmoid or polynomial

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm
# Visualising the Training set results
library(ggplot2)  # Assuming you want ggplot2 for visualizations

# Define grid of points for prediction
X1_seq <- seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2_seq <- seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1_seq, X2_seq)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

# Predict on the grid
y_grid <- predict(classifier, newdata = grid_set)
grid_set$Purchased <- as.factor(y_grid)  # Add predictions to grid data

# training set results
p<-ggplot() +
  geom_raster(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = Purchased), alpha = 0.5) +
  geom_point(data = training_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, show.legend=FALSE) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  scale_color_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  labs(title = 'Kernel SVM (Training set)', x = 'Age', y = 'Estimated Salary') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

# test set results
p<-ggplot() +
  geom_raster(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = Purchased), alpha = 0.5) +
  geom_point(data = test_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, show.legend=FALSE) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  scale_color_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  labs(title = 'Kernel SVM (Test set)', x = 'Age', y = 'Estimated Salary') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p