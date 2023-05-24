# Load required packages
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)
library(Metrics)
library(mlr)
library(ggplot2)
library (plotly)
library(magrittr)
library(caTools)
library(ggcorrplot)
library(corrplot)

# Load the dataset
setwd("D:\\ACADEMICS\\3RD YEAR COLLEGE\\3RD TERM\\Data Science 4\\Module 1\\SA")
data <- read.csv("D:\\ACADEMICS\\3RD YEAR COLLEGE\\3RD TERM\\Data Science 4\\Module 1\\SA\\Housing.csv")
#display
head(data)
dim(data)


#converting other variables to numeric form
data$mainroad <- as.numeric(factor(data$mainroad))
data$guestroom <- as.numeric(factor(data$guestroom))
data$basement <- as.numeric(factor(data$basement))
data$hotwaterheating <- as.numeric(factor(data$hotwaterheating))
data$airconditioning <- as.numeric(factor(data$airconditioning))
data$furnishingstatus <- as.numeric(factor(data$furnishingstatus))

#Removing irrelevant variables
data_clean <-data%>%
    select(-c(prefarea))
head(data_clean)
summary(data_clean)


#data splitting
create_split <- function(data_clean, size = 0.8, train = TRUE) {
  
  n_row = nrow(data_clean)
  total_row = size * n_row
  train_sample <- 1: total_row
  
  if (train ==TRUE){
    return(data_clean[train_sample, ])
  } else {
    return(data_clean[-train_sample, ])
  }
  
}

#Assigning of train and test data
train_set <- create_split(data_clean, 0.8, train=TRUE)
test_set <- create_split(data_clean, 0.8, train=FALSE)
dim(train_set)
dim(test_set)


#Decision tree creation
tree = rpart(train_set$price~., data=train_set)
rpart.plot(tree)


#Test prediction
predict_price <- predict(tree, test_set)
table_price <-table(test_set$price, predict_price)
print(table_price)


#Accuracy Test
accuracy_Test <- sum(diag(table_price)) / sum(table_price)
print(paste('Accuracy for test', accuracy_Test))


#Hyperparameter Tuning
accuracy_tune <- function(tree){
  predict_unseen <- predict(tree, test_set)
  table_mat <- table(test_set$parking, predict_unseen)
  accuracy_Test <-sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
  
}
control <- rpart.control (minsplit = 5, minbucket = round(5 /3), maxdepth = 3, cp = 0)


#Fine-Tuned results
tune_fit <-rpart(price~., data = train_set, control = control)
accuracy_tune(tune_fit)
rpart.plot(tune_fit)









