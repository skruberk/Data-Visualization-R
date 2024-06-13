# Support Vector Machine (SVM)
ibrary(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
library(class)
library(ElemStatLearn)
library(e1071) #SVM specific 
library(ISLR) #dataset from intro stat learning
# Importing the dataset
dataset = dataset[3:5]
View(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Split dataset into train/test
set.seed(123)
split<-initial_split(dataset) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
train_scaled <- scale(training_set[, -3])  # Scale only numerical features in the training set
test_scaled <- scale(test_set[, -3])    # Scale only numerical features in the test set
View(training_set)
View(test_set)
# Fitting SVM to the Training set

classifier <- svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# Define grid of points for prediction
set <- training_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

# Predict on the grid
y_grid <- predict(classifier, newdata = grid_set)
grid_set$Purchased <- y_grid  # Add predictions to grid data


# training set results
p<-ggplot() +
  geom_raster(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = Purchased), alpha = 0.5) +
  geom_point(data = training_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, show.legend=FALSE) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  scale_color_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  labs(title = 'SVM (Training set)', x = 'Age', y = 'Estimated Salary') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

# test set results
p<-ggplot() +
  geom_raster(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = Purchased), alpha = 0.5) +
  geom_point(data = test_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, show.legend=FALSE) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  scale_color_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  labs(title = 'SVM (Test set)', x = 'Age', y = 'Estimated Salary') +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

# Using the IRIS dataset --------------------------------------------------
#cost of constraints violation, permission to go beyond the margin, so influence or weight of the vector
#gamma for nonlinear kernel function, the free parameter of the gaussian radial function
#small gamma has large variance and the influence of the vector is larger
View(iris)
# Split dataset into train/test
set.seed(123)
split<-initial_split(iris) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)
model<-svm(Species~ .,data=training_set)
summary(model)
predictedvals <- predict(model,training_set[1:4])
table(predictedvals,training_set$Species)
tune_results<-tune(svm,train.x=iris[, 1:4],train.y=iris$Species,kernel='radial',ranges=list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
#tune does a grid search
summary(tune_results)
tuned<-svm(Species~ ., data=training_set,kernel='radial',cost=1.5,gamma=0.1)
summary(tuned)
