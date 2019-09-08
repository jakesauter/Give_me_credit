
load_filtered_training_data <- function() {
  df <- read.csv('./data/cs-training.csv') %>% 
    filter(DebtRatio < 3400) 
  df[,1] <- NULL
  df[1:(nrow(df) /2), ] %>% as_tibble
}

load_filtered_validation_data <- function() {
  df <- read.csv('./data/cs-training.csv') %>% 
    filter(DebtRatio < 3400) 
  df[,1] <- NULL
  df[((nrow(df)/2)+1): nrow(df), ] %>% as_tibble
}

load_all_training_data <- function() {
  df <- read.csv('./data/cs-training.csv')
  df[,1] <- NULL
  df
}

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
  
  ntrue <- length(which(test == 1))
  nfalse <- length(which(test == 0))
  
  tp <- length(which(preds == 1 & test == 1))
  fn <- length(which(preds == 0 & test == 1))
  
  tn <- length(which(preds == 0 & test == 0))
  fp <- length(which(preds == 1 & test == 0))
  
  
  results <- data.frame(true = c(tp, fn), false = c(fp, tn), row.names = c('pred_true', 'pred_false'))
  
  p1 <-
    plot_confusion(tp = tp, fp = fp, 
                 tn = tn, fn = fn) + 
    ggtitle('Total Counts') + 
    theme(title = element_text(face = 'bold', size = 12), 
          axis.title = element_text(face = 'plain', size = 12), 
          legend.title = element_text(face = 'plain'))
  
  p2 <-
    (results / sum(results)) %>% 
    round(3) %>% unlist %>% 
    plot_confusion(prop = TRUE) + 
    ggtitle('Proportional to Total') + 
    theme(title = element_text(face = 'bold', size = 12), 
          axis.title = element_text(face = 'plain', size = 12), 
          legend.title = element_text(face = 'plain'))
  
  results[,1] <- results[,1] / sum(results[,1])
  results[,2] <- results[,2] / sum(results[,2])
  
  p3 <-
    results %>% round(3) %>% unlist %>% 
    plot_confusion(prop = TRUE) + 
    ggtitle('Proportional to Class') + 
    theme(title = element_text(face = 'bold', size = 12), 
          axis.title = element_text(face = 'plain', size = 12), 
          legend.title = element_text(face = 'plain'))
  
  plt_grid <-
    plot_grid(p1, p2, p3)

  plt_grid
}

plot_confusion <- function(fp = 0, 
                           tn = 0, 
                           tp = 0, 
                           fn = 0, 
                           prop = FALSE) {
  
  if (length(fp) > 1) {
    tn <- fp[2]
    tp <- fp[3]
    fn <- fp[4]
    fp <- fp[1]
  }
  
  Y <- c(tn,fp,fn,tp)
  
  True <- factor(c(0,0,1,1), levels = c(0,1))
  Predicted <- factor(c(0,1,0,1), levels = c(1,0))

  df <- data.frame(True, Predicted, Y)
  
  plot <-
    ggplot(data = df, mapping = aes(x = True, y = Predicted)) +
    geom_tile(aes(fill = Y), colour = "white")  + 
    geom_text(aes(label = sprintf("%.2f", Y)), vjust = 1)
    
    
  if (prop) {
    plot <-
      plot +  
      scale_fill_gradient(low = "#0000FF",  high ="#FF0000", 
                          guide = "colourbar", limits = c(0, 1), 
                          name = element_text('Proportion', face = 'plain'))
  } else {
    plot <-
      plot + 
      scale_fill_gradient(low = "#0000FF",  high ="#FF0000", 
                        guide = "colourbar", limits = c(0, sum(tn, tp, fn, fp)), 
                        name = element_text('Count', face = 'plain'))
  }
    
  plot 
}
