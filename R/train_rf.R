#!/usr/bin/Rscript


## TODO: try 
# 1. Random Forest
# 2. SVM
# 3. Neural Net
# 4. ?
# K-fold cross validation of model after done playing
# Custom ROC implementation 
# to make use of all data

## TODO 
# 1. Read paper on tuning random forest and train better random forest
# 2. Implement k-fold cross validation 
# 3. (Done) Implement a simple AUC and learn better how to select
#    cutoff threshold for classifier from AUC curve


######################
# Random Forest
######################

library(randomForest)
library(magrittr)
library(dplyr)

# load in the data and filter from what we 
# have learned in data exploration phase
load_model_data()

xtest <- test %>% select(-SeriousDlqin2yrs)
ytest <- test$SeriousDlqin2yrs %>% as.factor


#############################################################################
# Random forest with 2500 trees
#============================================================================
cat('training rf model with 2500 trees ...\n')

start <- Sys.time()
rf_model_2500 <- randomForest(x = train %>% select(-SeriousDlqin2yrs), 
                              y = as.factor(train$SeriousDlqin2yrs), 
                              ntree = 2500,
                              xtest = xtest, 
                              ytest = ytest, 
                              keep.forest = TRUE)
stop <- Sys.time()

sink(file = 'rf_model_2500_train_time.txt')
print(stop-start)
sink()

saveRDS(rf_model_2500, file = 'rf_model_2500.rds')

raw_preds <- predict(rf_model_2500, 
                     test %>% select(-SeriousDlqin2yrs), 
                     type = 'prob')
preds <- raw_preds[,2] %>% unname
saveRDS(preds, 'rf_model_2500_preds.rds')
preds[preds > .5] = 1
preds[preds < .5] = 0
acc <- mean(preds == ytest)
cat('acc: ', acc, file = 'rf_model_2500_acc.txt')

#############################################################################
# Random forest with 5000 trees
#============================================================================
cat('training rf model with 5000 trees ...\n')

start <- Sys.time()
rf_model_5000 <- randomForest(x = train %>% select(-SeriousDlqin2yrs), 
                              y = as.factor(train$SeriousDlqin2yrs), 
                              ntree = 5000,
                              xtest = xtest, 
                              ytest = ytest, 
                              keep.forest = TRUE)
stop <- Sys.time()

sink(file = 'rf_model_5000_train_time.txt')
print(stop-start)
sink()

saveRDS(rf_model_5000, file = 'rf_model_5000.rds')

raw_preds <- predict(rf_model_5000, test %>% select(-SeriousDlqin2yrs), type = 'prob')
preds <- raw_preds[,2] %>% unname
saveRDS(preds, 'rf_model_5000_preds.rds')
preds[preds > .5] = 1
preds[preds < .5] = 0
acc <- mean(preds == ytest)
cat('acc: ', acc, file = 'rf_model_5000_acc.txt')

#############################################################################
# Random forest with 10000 trees
#============================================================================
cat('training rf model with 10000 trees ...\n')

start <- Sys.start()
rf_model_10000 <- randomForest(x = train %>% select(-SeriousDlqin2yrs), 
                               y = as.factor(train$SeriousDlqin2yrs), 
                               ntree = 10000,
                               xtest = xtest, 
                               ytest = ytest, 
                               keep.forest = TRUE)
stop <- Sys.start()

sink(file = 'rf_model_10000_train_time.txt')
print(stop-start)
sink()

saveRDS(rf_model_10000, file = 'rf_model_10000.rds')

raw_preds <- predict(rf_model_10000, test %>% select(-SeriousDlqin2yrs), type = 'prob')
preds <- raw_preds[,2] %>% unname
saveRDS(preds, 'rf_model_10000_preds.rds')
preds[preds > .5] = 1
preds[preds < .5] = 0
acc <- mean(preds == ytest)
cat('acc: ', acc, file = 'rf_model_10000_acc.txt')
#############################################################################




# confusion_matrix(test, preds, clf_thresh = .5)
# 
# roc <- simple_roc(test$SeriousDlqin2yrs, preds)
# 
# clf_thresh <- roc %>% select_threshold(FPR = .25)
# 
# confusion_matrix(test, preds, clf_thresh = clf_thresh)
















