# load in the data and filter from what we learned in data exploration phase

library(randomForest)
library(magrittr)
library(dplyr)

########################################################################################

########################################################################################

## TODO: try 
# 1. Random Forest
# 2. SVM
# 3. Neural Net
# 4. ?
# K-fold cross validation of model after done playing
# Custom ROC implementation 
# to make use of all data

######################
# Random Forest
######################

load_model_data()

train$SeriousDlqin2yrs %<>% as.factor()
xtest <- test %>% select(-SeriousDlqin2yrs)
ytest <- test$SeriousDlqin2yrs %>% as.factor

# 
# cat('training rf model with 2500 trees ...\n')
# 
# rf_model_2500 <- randomForest(SeriousDlqin2yrs ~ ., train, ntree = 2500,
#                          xtest = xtest, ytest = ytest, keep.forest = TRUE)
# 
# saveRDS(rf_model_2500, file = 'rf_model_2500.rds')
# 
# cat('training rf model with 5000 trees ...\n')
# 
# rf_model_5000 <- randomForest(SeriousDlqin2yrs ~ ., train, ntree = 5000,
#                            xtest = xtest, ytest = ytest, keep.forest = TRUE)
# 
# saveRDS(rf_model_5000, file = 'rf_model_5000.rds')
# 
# cat('training rf model with 10000 trees ...\n')
# 
# rf_model_10000 <- randomForest(SeriousDlqin2yrs ~ ., train, ntree = 10000,
                              # xtest = xtest, ytest = ytest, keep.forest = TRUE)
# 
# saveRDS(rf_model_10000, file = 'rf_model_10000.rds')

# rf_model <- readRDS('rf_model.rds')

rf_model <- randomForest(SeriousDlqin2yrs ~ ., train, ntree = 2,
                               xtest = xtest, ytest = ytest, keep.forest = TRUE)

raw_preds <- predict(rf_model, test %>% select(-SeriousDlqin2yrs), type = 'prob')

preds <- raw_preds[,2] %>% unname

confusion_matrix(test, preds, clf_thresh = .5)

roc <- simple_roc(test$SeriousDlqin2yrs, preds)

clf_thresh <- roc %>% select_threshold(FPR = .25)

confusion_matrix(test, preds, clf_thresh = clf_thresh)


## TODO read paper on tuning random forest and train better random forest, train
# svm, implement k-fold cross validation and AUC and learn better how to select
# cutoff threshold for classifier from AUC curve


# [1] 0.936031














