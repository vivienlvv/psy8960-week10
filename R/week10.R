# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(haven)
library(tidyverse)


# Data Import and Cleaning

## Dropping rows with missing value for hours worked across part time and 
## full time, NEED TO CONFIRM VARIABLE NAME
gss_tbl = read_sav("../data/GSS2016.sav") %>%  drop_na(HRS1)  %>% 
  rename(workhours = HRS1)

## 75% missingness flag 
retain_flag = sapply(gss_tbl, function(x) sum(is.na(x))/nrow(gss_tbl)) < 0.75

## Removing variables (columns) with less than 75% missingness 
gss_tbl = gss_tbl[, retain_flag]


# Visualization
gss_tbl %>% ggplot(aes(x = workhours)) + 
  geom_histogram(bins = 50) + 
  labs(y = "Frequency", x = "workhours",
       title = "Univariate distribution of workhours")


# Analysis 



# Publication

