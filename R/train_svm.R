#!/usr/bin/Rscript


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
library(magrittr)
library(dplyr)
library(e1071)


# load in the data and filter from what we 
# have learned in data exploration phase
load_model_data()

xtest <- test %>% select(-SeriousDlqin2yrs)
ytest <- test$SeriousDlqin2yrs %>% as.factor


#################################################################
# SVM with Linear Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'linear')
stop <- Sys.time()

sink(file = "svm_linear_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest)

saveRDS(preds, 'svm_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_linear_acc.txt')

#################################################################
# SVM with Polynomial Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'polynomial')
stop <- Sys.time()

sink(file = "svm_poly_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest)

saveRDS(preds, 'svm_poly_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_poly_acc.txt')

#################################################################
# SVM with Sigmoid Kernel 
#================================================================

start <- Sys.time()
model <- svm(x = train %>% select(-SeriousDlqin2yrs), 
             y = as.factor(train$SeriousDlqin2yrs), 
             kernel = 'sigmoid')
stop <- Sys.time()

sink(file = "svm_sigmoid_training_time.txt")
print(stop-start)
sink()

cat('total training time: ', stop-start)

preds <- predict(model, xtest)

saveRDS(preds, 'svm_sigmoid_preds.rds')

acc <- mean(preds == ytest)

cat('acc: ', acc, file = 'svm_sigmoid_acc.txt')

#################################################################



## TODO read paper on tuning random forest and train better random forest, train
# svm, implement k-fold cross validation and AUC and learn better how to select
# cutoff threshold for classifier from AUC curve
















