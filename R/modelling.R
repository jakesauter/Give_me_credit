# load in the data and filter from what we learned in data exploration phase


source('./load_data.R')

library(randomForest)
library(magrittr)
library(dplyr)

########################################################################################
load_model_data <- function() {
  df <- load_all_training_data()
  
  # Winsorize late payments
  
  mutate_vars <- c("NumberOfTime30.59DaysPastDueNotWorse",
                   "NumberOfTime60.89DaysPastDueNotWorse", 
                   "NumberOfTimes90DaysLate") 
  
  for (var in mutate_vars) {
    df[df[[var]] > 20, var] <- 20 
  }
  
  
  # Filter too high RUUL
  df <- df %>% 
    filter(RevolvingUtilizationOfUnsecuredLines < 10)
  
  # Fill in missing monthly incomes
  median_monthly_income <- df$MonthlyIncome[!is.na(df$MonthlyIncome)] %>% median
  df$MonthlyIncome[is.na(df$MonthlyIncome)] <- median_monthly_income
  
  # Fill in missing number of dependents
  median_num_dependents <- df$NumberOfDependents %>% .[!is.na(.)] %>% median
  df$NumberOfDependents[is.na(df$NumberOfDependents)] <- median_num_dependents
  
  # split into 70/30 training testing
  split <- .7 * nrow(df)
  train <<- df %>% filter(row_number() <= split)
  test <<- df %>% filter(row_number()  > split)
}

# ROC (tpr vs fpr)
# https://www.r-bloggers.com/roc-curves-in-two-lines-of-r-code/
# https://kennis-research.shinyapps.io/ROC-Curves/
simple_roc <- function(labels, scores, x11 = FALSE) {
  scores_order <- order(scores, decreasing = TRUE)
  labels <- labels[scores_order]
  df <- data.frame(TPR = cumsum(labels)/sum(labels), FPR = cumsum(!labels)/sum(!labels), labels, scores=scores[scores_order])
  if (x11) x11()
  plot <- df %>% ggplot + geom_point(aes(x = FPR, y = TPR))
  print(plot)
  df
}

select_threshold <- function(roc, FPR, thresh = 0.01) {
  stopifnot(!missing(roc) | !missing(FPR))
  roc <- roc[(roc$FPR > (FPR - thresh) & roc$FPR < (FPR + thresh)), 'scores']
  cat('range within thresh: ', range(roc), '\n')
  selected_thresh <- median(roc)
  cat('median threshold: ', selected_thresh)
  selected_thresh
}

confusion_matrix <- function(test, preds, clf_thresh) {
  
  if (!is.null(preds)) {
    preds[preds > clf_thresh] = 1
    preds[preds < clf_thresh] = 0
  }
  
  ntrue <- length(which(test$SeriousDlqin2yrs == 1))
  nfalse <- length(which(test$SeriousDlqin2yrs == 0))
  
  tp <- length(which(preds == 1 & test$SeriousDlqin2yrs == 1))
  fn <- length(which(preds == 0 & test$SeriousDlqin2yrs == 1))
  
  tn <- length(which(preds == 0 & test$SeriousDlqin2yrs == 0))
  fp <- length(which(preds == 1 & test$SeriousDlqin2yrs == 0))
  
  
  results <- data.frame(true = c(tp, fn), false = c(fp, tn), row.names = c('pred_true', 'pred_false'))
  
  cat('\nnumber of predictions confusion matrix: \n')
  results %>% print
  
  cat('\n')
  
  cat('proprtional to population confustion matrix: \n')
  (results / sum(results)) %>% round(3) %>%  print
  
  cat('\n')
  
  results[,1] <- results[,1] / sum(results[,1])
  results[,2] <- results[,2] / sum(results[,2])
  
  cat('proportional to category confustion matrix: \n')
  results %>% round(3) %>% print
  
  cat('\n')
  
  tpr = tp / (tp + fn)
  tnr = tn / (tn + fp)
  
  cat('true positive rate: ', tpr, '\n')
  cat('true negative rate: ', tnr, '\n')
  
  cat('\n')
}
########################################################################################


load_model_data()

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














