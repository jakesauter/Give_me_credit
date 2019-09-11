#!/usr/bin/Rscript


library(randomForest)
library(magrittr)
library(dplyr)

## TODO: try 
# 1. Random Forest
# 2. SVM
# 3. Neural Net
# 4. ?
# K-fold cross validation of model after done playing
# Custom ROC implementation 
# to make use of all data

#########################
# Support Vector Machine
#########################

library(dplyr)
library(magrittr)

# load in the data and filter from what we learned in data exploration phase 
load_model_data()

xtest <- test %>% select(-SeriousDlqin2yrs)
ytest <- test$SeriousDlqin2yrs %>% as.factor

library(e1071)

#################################################################
# SVM with Linear Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'linear', 
             probability = TRUE)
stop <- Sys.time()

sink(file = "svm_linear_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest, probability = TRUE)

saveRDS(preds, 'svm_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_linear_acc.txt')

#################################################################
# SVM with Polynomial Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'polynomial', 
             probability = TRUE)
stop <- Sys.time()

sink(file = "svm_poly_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest, probability = TRUE)

saveRDS(preds, 'svm_poly_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_poly_acc.txt')

#################################################################
# SVM with Sigmoid Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'sigmoid', 
             type = 'C-classification',
             probability = TRUE)
stop <- Sys.time()

sink(file = "svm_sigmoid_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest, probability = TRUE)

saveRDS(preds, 'svm_sigmoid_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_sigmoid_acc.txt')

#################################################################



## TODO read paper on tuning random forest and train better random forest, train
# svm, implement k-fold cross validation and AUC and learn better how to select
# cutoff threshold for classifier from AUC curve
















