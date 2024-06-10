# Logistic Regression, calculating maximum likelihood
library(caTools)
library(ElemStatLearn)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(GGally)
library(caTools)
library(tidymodels)
dataset = dataset[3:5]

# Splitting the dataset into the Training set and Test set

set.seed(123)
split<-initial_split(dataset) #default 3/4 split
training_set<-training(split)
test_set<-testing(split)
View(training_set)
#feature scaling, but scale only numerical features not the response variable
numerical_columns <- sapply(training_set[, -3], is.numeric)
numerical_columns <- c(numerical_columns, FALSE)
#only numerical columns
training_set[, numerical_columns] <- scale(training_set[, numerical_columns])
test_set[, numerical_columns] <- scale(test_set[, numerical_columns])
View(training_set)
# Fitting Logistic Regression to the Training set, glm generalized linear model
classifier<-glm(formula = Purchased ~ .,
                 family = binomial(link="logit"), #error distribution, gaussian(link = "identity"),quasibinomial(link = "logit")
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm <- table(test_set$Purchased, y_pred > 0.5)
View(cm)
# Visualising the Training set results

set <- training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) #build a grid -1 and +1 so that points aren't on the boundary
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2) #grid of all combos of x1 and x2 vals
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set) #predicts probabilities of class membership
y_grid = ifelse(prob_set > 0.5, 1, 0) #convert predicted probabilities into binary
plot(set[, -3], #scatter plot without Purchased
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) #adds the decision boundary to plot
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))
