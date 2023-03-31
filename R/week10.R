# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(haven)
library(tidyverse)
library(caret)
## Setting seed for reproducibility
set.seed(2023)



# Data Import and Cleaning

## Using a single pipe, I did the following: 
## 1) read in the .sav file,
## 2) mutated columns to be numeric to get rid of meta-data; 
## 3) dropped NAs in the required column; 
## 4) renamed variable to workhours; 
## 5) selected only cols with less than 75% of missing data using  select_if()
## because it allows selection of variables with logical vector as entry

gss_tbl = read_sav("../data/GSS2016.sav") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na(HRS1) %>% 
  # Double check missing value cleaning
  rename(workhours = HRS1) %>% 
  select_if(sapply(., FUN = function(x) sum(is.na(x))/nrow(.) < 0.75))




# Visualization

## Using ggplot2 inside tidyverse, I created a histogram to look at the 
## distributions of workhours 
gss_tbl %>% ggplot(aes(x = workhours)) + 
  geom_histogram(bins = 50) + 
  labs(x = "workhours", y = "Frequency",
       title = "Univariate distribution of workhours")




# Analysis

## Creating train and test splits 
index = createDataPartition(gss_tbl$workhours, p = 0.75, list = FALSE)
gss_tbl_train = gss_tbl[index,]
gss_tbl_test = gss_tbl[-index,]

## Creating 10 folds used in cross-validation from training set
training_folds = createFolds(gss_tbl_train$workhours, 10)

## Creating reusable trainControl object for all 4 models 
reuseControl = trainControl( method = "cv", number = 10, search = "grid", 
                             indexOut = training_folds, verboseIter = TRUE)


## Training models 
### Notes: 
### 1. Grid search was not given a custom tuning grid because I had no reason
### to think certain hyperparameter values would work better than others 
### 2. center, scale, nzv are only applied to linear models (OLS & glmnet) 
### because tree-based models can normally deal with standardization and 
### variance in variables 


### 1. OLS Regression (lm)
mod_ols = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "lm",
                metric = "Rsquared",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = c("center", "scale", "nzv", "medianImpute"))

## 2. Elastic Net (glmnet)
mod_enet = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "glmnet",
                metric = "Rsquared",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = c("center", "scale", "nzv", "medianImpute"))

## 3. random forest (rf)
mod_rf = train(workhours ~ ., 
               data = gss_tbl_train,
               method = "ranger",
               metric = "Rsquared",
               na.action = na.pass,
               trControl = reuseControl,
               preProcess = "medianImpute")

## 4. XGBoost (xgbTree)
mod_xgb = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "xgbTree",
                metric = "Rsquared",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = "medianImpute")



# Publication- NEED TO ADD STUFF IN

## Responses:

### 1. Across the four algorithms, we can see that tree-based models (i.e.,
### random forest and xgboost) yield better performance than linear models (i.e.,
### OLS and elastic net) as shown by the larger R squared values from both 
### 10-fold cross-validation and holdout validation. One potential explanation 
### is that [ADD STUFF HERE]

### 2. Compared to results from 10-fold cross-validation, the R-Squared value 
### for holdout validation set is significantly smaller. This is likely because
### of potential overfitting issues during model training 

### 3. Among the four models, I will choose random forest. Random forest and
### XGBoost both had the greatest cross-validated R2 value which means 
### they were able to explain the greatest amount of variance in 
### work hours, our response variable. Also, random forest has the greatest 
### holdout sample validation R2 value which means that the model generalizes
### to unseen "new" data the best and is able to make best predictions 
### with least error compared to, say, xgboost. 


## Creating a results function to get required info from each model
### R2 was computed as squared correlation based on lecture demo
results = function(train_mod){
  
  algo = train_mod$method
  cv_rsq = str_remove(format(round(max(train_mod$results$Rsquared), 2), nsmall = 2), "^\\d")
  preds = predict(train_mod, gss_tbl_test, na.action = na.pass)
  ho_rsq = str_remove(format(round(cor(preds, gss_tbl_test$workhours)^2, 2), nsmall = 2), "^\\d")
  
  return(c(algo, cv_rsq, ho_rsq))
}

## Constructing required tibble
table1_tbl = as_tibble(rbind(results(mod_ols), 
                             results(mod_enet),
                             results(mod_rf), 
                             results(mod_xgb))) 
colnames(table1_tbl) = list("algo", "cv_rsq", "ho_rsq")


# 3/31- 18:47
# algo    cv_rsq ho_rsq
# <chr>   <chr>  <chr> 
#   1 lm      .70    .29   
# 2 glmnet  .73    .35   
# 3 ranger  .93    .54   
# 4 xgbTree .93    .46  
