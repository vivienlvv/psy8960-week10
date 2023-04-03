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
## 3) Confirmed all missing values/ non-applicable/ don't know, etc. were
## imported as NAs
## 4) dropped NAs in the required column HRS1; 
## 5) renamed variable to workhours; 
## 6) selected only cols with less than 75% of missing data using  select_if()
## because it allows selection of variables with logical vector as entry

gss_tbl = read_sav("../data/GSS2016.sav") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na(HRS1) %>% 
  # Double check missing value cleaning
  rename(workhours = HRS1) %>% 
  select_if(sapply(., FUN = function(x) sum(is.na(x))/nrow(.) < 0.75))

## Creating vector containing info for each method for Steps 7 & 8 
mod_vec = c("lm", "glmnet", "ranger", "xgbTree")




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

### Comments to explain model training decisions: 
### 1. Grid search was not given a custom tuning grid because I had no reason
### to think specific hyperparameter values would work better than others, so 
### I decided to just use the default tune grid provided by caret
### 2. center, scale, nzv are only applied to linear models (OLS & glmnet) 
### because tree-based models can normally deal with standardization and 
### variance in variables 
### 3. Models were optimized on R2 because assignment instructions seemed to 
### suggest this


## Creating an empty list to store output models from for loop
mod_ls = list()

for(i in 1:length(mod_vec)){
  
  method = mod_vec[i]
  
  # Getting pre-processing options based on method used
  if(method == "lm" | method == "glmnet"){
    pre_process = c("center", "scale", "nzv", "medianImpute")
  }else{
    pre_process = "medianImpute"
  }
  
  # Training model 
  mod = train(workhours ~ .,
              data = gss_tbl_train,
              method = method,
              metric = "Rsquared",
              na.action = na.pass,
              trControl = reuseControl,
              preProcess = pre_process)
  
  # Saving model from each iteration to the pre-defined list 
  mod_ls[[i]] = mod
  
}

# Publication

## Question Responses:
### 1. How did your results change between models? Why do you think this happened, specifically?

### Across the four algorithms, we can see that tree-based models (i.e.,
### random forest and xgboost) yield better performance than linear models (i.e.,
### OLS and elastic net) as shown by the larger R squared values from both 
### 10-fold cross-validation and holdout validation. One potential explanation 
### is that there are potential higher order effects across variables that 
### cannot be captured using linear models. 

### 2. How did you results change between k-fold CV and holdout CV? Why do you think this happened, specifically?

### Compared to results from 10-fold cross-validation, the R-Squared values 
### for holdout validation set are substantially smaller. This is likely because
### of potential overfitting during model training. 

### 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.

### Among the four models, I will choose random forest. Random forest and
### XGBoost  had the greatest cross-validated R2 values which means 
### they were able to explain the greatest amount of variance in 
### work hours, our response variable. But random forest has the greatest 
### holdout sample validation R2 value which means that the model generalizes
### to unseen "new" data the best and is able to make 
### predictions based on new observations with least error compared to the 
### other three algorithms. Therefore, with prediction as the final goal,
### I will choose random forest. 


## Constructing required tibble

## Creating a results function to get required info from each model
### R2 was computed as squared correlation based on lecture demo
### Input: caret train model; Output: vector of required info 
### Note that this function can be passed directly to sapply() function below,
### but for clarity, I decided to do them separately
results = function(train_mod){
  algo = train_mod$method
  cv_rsq = str_remove(format(round(max(train_mod$results$Rsquared), 2), nsmall = 2), "^\\d")
  preds = predict(train_mod, gss_tbl_test, na.action = na.pass)
  ho_rsq = str_remove(format(round(cor(preds, gss_tbl_test$workhours)^2, 2), nsmall = 2), "^\\d")
  return(c("algo" = algo, "cv_rsq" = cv_rsq, "ho_sq" = ho_rsq))
}

table1_tbl = as_tibble(t(sapply(mod_ls, results)))


# Output
# A tibble: 4 × 3
# algo    cv_rsq ho_sq
# <chr>   <chr>  <chr>
# 1 lm      .71    .29  
# 2 glmnet  .75    .32  
# 3 ranger  .91    .54  
# 4 xgbTree .93    .48   
