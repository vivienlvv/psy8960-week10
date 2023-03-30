# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(haven)
library(tidyverse)
library(caret)

# Data Import and Cleaning

## Dropping rows with missing value for hours worked across part time and 
## full time, NEED TO CONFIRM VARIABLE NAME
gss_tbl = read_sav("../data/GSS2016.sav") %>% 
  drop_na(HRS1) %>% 
  rename(workhours = HRS1)

## 75% missingness flag 
retain_flag = sapply(gss_tbl, function(x) sum(is.na(x))/nrow(gss_tbl)) < 0.75

## Removing variables (columns) with less than 75% missingness 
gss_tbl = gss_tbl[, retain_flag]

## Getting rid of SPSS meta-data 
gss_tbl = data.frame(lapply(gss_tbl, as.numeric))




# Visualization
gss_tbl %>% ggplot(aes(x = workhours)) + 
  geom_histogram(bins = 50) + 
  labs(y = "Frequency", x = "workhours",
       title = "Univariate distribution of workhours")


# Analysis

## Setting seed for reproducibility
set.seed(2023)

## Requirements:
## 7.1 Training models 
## 7.2 grid search with 10-fold CV on 75/25 split 
## 7.3 median imputation for pre-processing 
## 7.4 get error estimates for 10fold CV and holdout


## Creating train and test splits 
index = createDataPartition(gss_tbl$workhours, p = 0.75, list = FALSE)
gss_tbl_train = gss_tbl[index,]
gss_tbl_test = gss_tbl[-index,]

## Creating 10 folds used in cross-validation 
training_folds = createFolds(gss_tbl_train$workhours, 10)

## Creating reusable trainControl object for all models 
reuseControl = trainControl( method = "cv", number = 10, search = "grid", indexOut = training_folds, verboseIter = TRUE)

## 1. OLS Regression (lm)
mod_ols = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "lm",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = c("center", "scale", "nzv", "medianImpute"))

## 2. Elastic Net (glmnet)
# enet_grid = expand.grid(alpha = seq(0, .5, by = .125),lambda = seq(0, 1, by = .25))
mod_enet2 = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "glmnet",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = c("center", "scale", "nzv", "medianImpute"))

## 3. random forest (rf)
# rf_grid = expand.grid(mtry, min.node.size, splitrule)
mod_rf = train(workhours ~ ., 
               data = gss_tbl_train,
               method = "ranger",
               na.action = na.pass,
               trControl = reuseControl,
               preProcess = "medianImpute")

## 4. XGBoost (xgboost)
# xgb_grid = expand.grid(max_depth, nrounds (i.e., ntrees), eta, gamma, subsample, min_child_weight, colsample_bytree)

mod_xgb = train(workhours ~ ., 
                data = gss_tbl_train,
                method = "xgbTree",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = "medianImpute")


## Next time: Need to figure out hyperparams to tune + make grids 





# Publication

