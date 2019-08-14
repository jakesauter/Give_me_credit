library(ggplot2)

##########################################
# TODO
##########################################
# Might be a good idea to turn this into 
# an Rmarkdown file, though for now 
# this pseudo notebook like working 
# will be fine
##########################################

df <- load_all_training_data()

# Frist lets take a look at how balanced our dataset is
df$SeriousDlqin2yrs %>% mean

# We see that the mean of this set of binary labels is 
# about .066, showing that the dataset is mostly filled 
# with people who did not default on their loans 

# Plot the distributions of people who defaulted vs age, 
# in general we should see that younger people default more
# as credit agencies must give mortgage to those who are
# 55 and older 
ggplot(df) +
  geom_histogram(aes(x = age), bins = 20) + 
  facet_wrap(~SeriousDlqin2yrs)

# Now lets get a 5 number summary of the debt ratio
summary(df$DebtRatio)

# same thing as rounded quantiles 
df$DebtRatio %>% quantile %>% round(2)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0      0.2      0.4    360.9      0.9 329664.0 

# Its concerning that we see that someone owes 300,000 times
# more than they own, is this person a single outlier?

df$DebtRatio %>% quantile(.975)

# 97.5% 
# 3521.025 

# From this statistic we can see that 2.5% of the dataset owes more 
# than 3,500 times what they own. We need to investigate further
# to see if these are outliers or not 

df %>% 
  filter(DebtRatio > 3489.025) %>% 
  select(DebtRatio, MonthlyIncome) %>% 
  summary


# We see that these outliers either have a monthly income
# of 0, 1 or NA

# This seems like it could be a data-entry error, lets see if 
# there is any correlation between Serious Deliquency rate 
# and the monthly income for these examples


corr <- df %>% 
  filter(DebtRatio > 3489.025 & !is.na(MonthlyIncome)) %>% 
  select(SeriousDlqin2yrs, MonthlyIncome) %>% 
  mutate(corr = SeriousDlqin2yrs == MonthlyIncome) %>% 
  select(corr) %>% 
  unlist %>% unname


corr %>% 
  table

corr %>% mean

# We see that out of the 184 samples that have a valid
# monthly income, 164 of them (88.6%) have the same value for 
# Seriously Deliquent as for monthly income, so this
# must be a data entry error 

# We see this corrolation, but are these people defaulting on
# their credit more than usual? 

# 0   1   4  11 
# 177   6   1   1 

# It doesn't make sense that these people owe 3500 times more
# than they own and they do not default on their credit, 
# these samples also seem to have data entry error so 
# should probably be removed

# I don't know -- I follow their logic here but these could
# be people that bankrupted and are making steady 
# affordable payments for themselves -- at any rate 
# it does seem much like an outlier case 
# and can be filtered out before modelling 
# as these are stand out cases that the model
# should probably not be predicting anyway 

df <- df %>% 
  filter(!is.na(MonthlyIncome) && DebtRatio < 3489.025)


# Now lets take a look at the number of 90 days late variable

df %>%
  select(NumberOfTimes90DaysLate) %>% 
  unlist %>% 
  table 

# 0      1      2      3      4      5      6      7      8      9     10     11     12     13 
# 141662   5243   1555    667    291    131     80     38     21     19      8      5      2      4 
# 14     15     17     96     98 
# 2      2      1      5    264 

# It is interesting that no one is between 17 and 96 times late. Lets take 
# a lot at these records

df %>%
  filter(NumberOfTimes90DaysLate > 95) %>% 
  select(SeriousDlqin2yrs, 
         NumberOfTime30.59DaysPastDueNotWorse, 
         NumberOfTime60.89DaysPastDueNotWorse, 
         NumberOfTimes90DaysLate) %>% 
  summary

# This sample of subjects has the same exact number of times
# late for all three recorded categories, though this might
# not be an entry error as they do have a very high default
# rate at around 55%, compared to the popululation seen
# above at 6%


# This might be a good time to implement Winsorization
# https://en.wikipedia.org/wiki/Winsorizing -- The act of 
# transforming statistics by limiting extreme values to 
# reduce the effect of outliers

# I can actually think of something I am doing at work 
# as Winsorization (even though it is just correcting
# invalid inf values) -- by setting the snr of signals
# with infinite snr to max_snr + 1

# This has the same effect as amplitude clipping 
# in signal processing 

# To implmenent winsorization here we will let these 
# number of defauling times be 20s instead of 96 and 98 

mutate_vars <- c("NumberOfTime30.59DaysPastDueNotWorse",
                 "NumberOfTime60.89DaysPastDueNotWorse", 
                 "NumberOfTimes90DaysLate") 


for (var in mutate_vars) {
  df[df[[var]] > 20, var] <- 20 
}


# mutate_vars %<>% syms 
# for (var in mutate_vars) {
#   df %<>% 
#     mutate(!!var := ifelse(!!var > 20, 20, !!var))
# }

# R snippet, this is how we could do this with a function 
# instead of in a loop 
# 
# test <- function(df,var) {
#   var <- enquo(var)
#   df %>%
#     mutate(var = ifelse(!!var > 20, 20, !!var)) %>% 
#     print
#   
# }
# might need the := assignment operator but unsure 
# 
# df %>% test(NumberOfTime30.59DaysPastDueNotWorse)

# Also could have just used base R and strings, 
# this way was way simpler and is probably how 
# I would do it if I had to mutate variables 
# again 

# for (var in mutate_vars) {
#   df[df[[var]] > 20, var] <- 20 
# }


# The last variable that we are going to take a look at is 
# revolving utilization of unsecured lines (RUUL)

# RUUL represents the ratio of money owed to credit limit, 
# so it should really not be above 1

# More on Revovling Utilization here
# https://finance.yahoo.com/news/credit-101-revolving-utilization-060108050.html

df %>% 
  select(RevolvingUtilizationOfUnsecuredLines) %>% 
  summary

# Woah, we see a vlues of 50,708. Something is most likely 
# wrong with this entry. Lets first examine different levels
# of credit and how much they are defaulting 

x <- df %>% 
  select(RevolvingUtilizationOfUnsecuredLines, 
         SeriousDlqin2yrs) %>% 
  filter(RevolvingUtilizationOfUnsecuredLines >= .9, 
         RevolvingUtilizationOfUnsecuredLines < 4)

x %>% count
x %>% summary


# These entries have a default rate of ~ 22.5% -- almost 1 in 4

# Lets consider 4 to 10 as well

x <- df %>% 
  select(RevolvingUtilizationOfUnsecuredLines, 
         SeriousDlqin2yrs) %>% 
  filter(RevolvingUtilizationOfUnsecuredLines > 4, 
         RevolvingUtilizationOfUnsecuredLines <= 10)

x %>% count
x %>% summary

# There are only 23 records in this region, but they are defaulting
# at a very high rate, with this in mind lets take a look at records
# with an even higher RUUL

x <- df %>% 
  select(RevolvingUtilizationOfUnsecuredLines, 
         SeriousDlqin2yrs) %>% 
  filter(RevolvingUtilizationOfUnsecuredLines > 10) 

x %>% count
x %>% summary

# But how can people have more 
# debt than their credit limit?? -- It could be from people
# maxxing out a credit card and having it closed -- or if they
# do not make an income and have a debt -- their account income
# might be set to $0.01 instead of 0 as to not lead to a Nan value. 

# After investigating further we see that these 241 people are not defaulting
# at any higher of a rate than the standard population. Again this is a situation
# that we might want to filter out before classification through an svm
# as this might throw off our classifier. 

df <- df %>% 
  filter(RevolvingUtilizationOfUnsecuredLines < 10)


# Finally lets take a look at using regression to fill in our 
# missing values for Monthly Income as this seems to be an important
# feature that we would want to have for every sample

clean_df <- df %>% 
  drop_na

model <- lm(MonthlyIncome~., clean_df)

model$residuals %>% range


model$residuals %>% 
  as.data.frame() %>% 
  ggplot + 
  geom_histogram(aes(x = .))

model$coefficients

# summary is overridden for model types so we can see the r squared there

model %>% summary

# Our adjusted R squared value (penalizes for model size) is .022 -- only
# slightly better than guessing the mean

# We see that we have a very significant F-statistic, though this does not
# automatically mean that our model is amazing -- Just because we have
# statistical significance does not mean that we have practical significance


# In short, this is bad so we will replace missing values 
# with median value (robust to outliers)


median_monthly_income <- df$MonthlyIncome[!is.na(df$MonthlyIncome)] %>% median

df$MonthlyIncome[is.na(df$MonthlyIncome)] <- median_monthly_income

# game changer syntax
x <- df$MonthlyIncome %>% .[. < quantile(., .95)]

df %>% 
  as.data.frame %>% 
  ggplot +
  geom_histogram(aes(x = .), bins = 100)

x %>% table

# 30k entries did not have a monthly income so thats why so many
# people have the median value that we calculated 

# This random forest is actually very small, using only 500 trees. The guide 
# above mentioned that 1000-5000 trees may be used for harder cases and if there
# are a lot of features. 

# This leads me to having a computation issue on my small laptop. I could run a 
# google vm, but this would lead me to having to set it up on the vm and if I wanted
# to do the same thing for another project then I would have to set it up all over again.

# there is an option to add a docker container so I think that I should setup a docker
# container for this purpose so that I don't waste time installing R and making sure 
# that everything is setup. Acutally now thinking about it I only have to install 
# R to to get what I want to work to work, but I would also like Rstudio downloaded
# and I don't think it is wise to wase vm runtime getting the vm setup and not 
# computing. So the next thing that I will get into is docker containers 

# for now I will see if I can get kaggle kernels to work for me





















