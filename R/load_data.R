

load_filtered_training_data <- function() {
  df <- read.csv('../data/cs-training.csv') %>% 
    filter(DebtRatio < 3400) 
  df[,1] <- NULL
  df[1:(nrow(df) /2), ] %>% as_tibble
}

load_filtered_validation_data <- function() {
  df <- read.csv('../data/cs-training.csv') %>% 
    filter(DebtRatio < 3400) 
  df[,1] <- NULL
  df[((nrow(df)/2)+1): nrow(df), ] %>% as_tibble
}

load_all_training_data <- function() {
  df <- read.csv('../data/cs-training.csv')
  df[,1] <- NULL
  df
}
