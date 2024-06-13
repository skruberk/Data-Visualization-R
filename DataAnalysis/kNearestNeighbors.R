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
library(ISLR)

head(Caravan)
str(Caravan) #get the structure
summary(Caravan$Purchase)
any(is.na(Caravan))
# Encoding the target feature as factor, 
var(Caravan[,1])
var(Caravan[,3])
purchase <- factor(Caravan$Purchase, levels = c("No", "Yes"), labels = c(0, 1))
Caravan1<-select(Caravan,-Purchase)
stand_car<-scale(Caravan1)
stand_car <- as.data.frame(stand_car)
#scale of variable for Knn really matters
var(stand_car[,1])
var(stand_car[,3]) #check if scaled 
dataset <- cbind(stand_car, purchase = as.numeric(as.character(purchase))) #cbind coerces into all same type so need to fix
dataset$purchase <- factor(dataset$purchase, levels = c(0, 1)) #fixes cbind coersion 

# Splitting the dataset into the Training set and Test set
set.seed(123)
split<-initial_split(dataset) #default 3/4 split
training_set<-training(split) #splits rows, not across columns here 
test_set<-testing(split)
train_p<-training_set$purchase
test_p<-test_set$purchase

# Fitting K-NN, predict test set, convert predicted probabilities into binary
predict_p <- knn(training_set,test_set,training_set$purchase,
             k = 7,
             prob = TRUE)
error_rate<-mean(test_p != predict_p)

# calculate K value and visualize -----------------------------------------
predict_p<-NULL
error_rate<-NULL
for (i in 1:20){
  set.seed(101)
  predict_p<-knn(training_set,test_set,training_set$purchase,k = i)
  error_rate[i]<-mean(test_p != predict_p)
}
print(error_rate)
#visualization 
kvals<-1:20
error_df<-data.frame(error_rate,kvals)
p<-ggplot(error_df,aes(kvals,error_rate))+geom_point()+geom_line(lty='dotted',color='blue')+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title = 'Calculate Optimal k value', x = 'k values', y = 'Error Rate') 
p


# Example Plot ------------------------------------------------------------
# Define the range of values for vars

# Define the range of values for the selected features
X1 <- seq(min(training_set$MOSTYPE) - 1, max(training_set$MOSTYPE) + 1, by = 0.1)
X2 <- seq(min(training_set$MZFONDS) - 1, max(training_set$MZFONDS) + 1, by = 0.1)

# Create a grid of points
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('MOSTYPE', 'MZFONDS')

# Predict the class for each point in the grid
predict2 <- knn(train = training_set[, c('MOSTYPE', 'MZFONDS')], 
                test = grid_set, 
                cl = train_p, 
                k = 7)

# Combine grid points and predictions into a data frame
grid_set$Predicted <- as.numeric(as.character(predict2))

# Plot
p <- ggplot() +
  geom_point(data = grid_set, aes(x = MOSTYPE, y = MZFONDS, color = factor(Predicted)), alpha = 0.3) +
  geom_point(data = training_set, aes(x = MOSTYPE, y = MZFONDS, color = factor(purchase)), size = 2) +
  labs(title = 'K-Nearest Neighbors (Training set)',
       x = 'MOSTYPE', y = 'MZFONDS') +
  scale_color_manual(values = c("salmon", "dodgerblue"), 
                     name = 'Class',
                     labels = c('No', 'Yes')) +
  theme_minimal()
p


# training set
set <- training_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

# Convert numeric predictions to factor for plotting
predict_p <- as.factor(predict_p)

# Create a data frame for plotting
plot_data <- data.frame(grid_set, Predicted = y_grid)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = Age, y = EstimatedSalary)) +
  geom_point(aes(color = Predicted), alpha = 0.5) +  # Plot points colored by prediction
  geom_point(data = set, aes(shape = factor(Purchased), fill = factor(Purchased)), color = 'black', size = 3) +  # Original data points
  geom_contour(aes(z = as.numeric(Predicted)), bins = 1, color = 'black', alpha = 0.5) +  # Decision boundary
  labs(title = 'K-Nearest Neighbors (Training set)', x = 'Age', y = 'Estimated Salary') +
  scale_fill_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  scale_shape_manual(values = c("0" = 21, "1" = 21)) +
  scale_color_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  theme_minimal()
p
# test set
set <- test_set
p <- ggplot(plot_data, aes(x = Age, y = EstimatedSalary)) +
  geom_point(aes(color = Predicted), alpha = 0.5) +  # Plot points colored by prediction
  geom_point(data = set, aes(shape = factor(Purchased), fill = factor(Purchased)), color = 'black', size = 3) +  # Original data points
  geom_contour(aes(z = as.numeric(Predicted)), bins = 1, color = 'black', alpha = 0.5) +  # Decision boundary
  labs(title = 'K-Nearest Neighbors (Training set)', x = 'Age', y = 'Estimated Salary') +
  scale_fill_manual(values = c("0" = "salmon3", "1" = "dodgerblue3")) +
  scale_shape_manual(values = c("0" = 21, "1" = 21)) +
  scale_color_manual(values = c("0" = "salmon", "1" = "dodgerblue")) +
  theme_minimal()
p