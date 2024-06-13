#clean data and fill in missing values for logistic regression
# More logistic regression ------------------------------------------------
titan_test<-titanic_test
titan_train<-titanic_train
#see and fill missing data
missmap(titan_train, col=c('yellow','black'),legend=FALSE)
p<-ggplot(titan_train,aes(Pclass)) +geom_bar(aes(fill=factor(Pclass)))
p
p<-ggplot(titan_train, aes(Pclass, Age))+geom_boxplot(aes(group=Pclass, fill=Pclass))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
#get the means
titan_mean <- titan_train %>%
  group_by(Pclass) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
  )
View(titan_mean)

fill_age <- function(age, Pclass) {
  out <- age
  for (i in 1:length(age)) {
    if (is.na(age[i])) {
      if (Pclass[i] == 1) {
        out[i] <- 38
      } else if (Pclass[i] == 2) {
        out[i] <- 30
      } else {
        out[i] <- 25
      }
    } else {
      out[i] <- age[i]
    }
  }
  return(out)
}
titan_train$full_age<-fill_age(titan_train$Age,titan_train$Pclass)
titan_train_fill <- cbind(titan_train,full_age)
titan_train_fill <- select(titan_train_fill,-Age,-PassengerId,-Name,-Ticket)
View(titan_train_fill)
missmap(titan_train_fill, col=c('yellow','black'))
t_train<-titan_train_fill
str(t_train) #shows all the data columns and ids
t_train$Survived <- factor(t_train$Survived)
t_train$Pclass <- factor(t_train$Pclass)
t_train$SibSp <- factor(t_train$SibSp)
t_model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = t_train)
summary(t_model)
#do the same for the test set
titan_test$full_age<-fill_age(titan_test$Age,titan_test$Pclass)
View(titan_test)
titan_test <- select(titan_test,-Age,-Cabin,-PassengerId,-Name,-Ticket)
missmap(titan_test, col=c('yellow','black'))
str(titan_test) #shows all the data columns and ids
titan_test$Pclass <- factor(titan_test$Pclass)
titan_test$Parch <- factor(titan_test$Parch, levels = levels(t_train$Parch))
titan_test$SibSp <- factor(titan_test$SibSp)

t_predict<-predict(t_model,newdata=titan_test,type='response') #generates probabilities for survival 
t_results<-ifelse(t_predict>0.5,1,0) #greater than 50% =1 less than 50% equals 0
titan_test$Survived <- t_results
View(titan_test)
p<-ggplot(titan_test, aes(x = factor(Survived), fill = Survived)) +
  geom_bar(position = 'dodge', show.legend=FALSE) +
  labs(x = 'Predicted Survival', y = 'Count', fill = 'Predicted Survival') +
  ggtitle('Actual vs Predicted Survival') +
  theme_minimal()
p
plot_train <- ggplot(t_train, aes(x = factor(Survived), fill = factor(Survived))) +
  geom_bar(show.legend=FALSE) +
  labs(x = 'Actual Survival', y = 'Count', fill = 'Predicted Survival')+
  theme_minimal()
plot_train
p <- plot_test + xlab('Predicted Survival')
plot_train <- plot_train + xlab('Actual Survival')
# Arrange plots side by side
p + plot_train + plot_layout(ncol = 2)


