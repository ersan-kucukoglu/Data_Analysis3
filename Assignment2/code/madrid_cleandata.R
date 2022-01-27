# CLEAR MEMORY
rm(list=ls())
#import libraries
library(data.table)
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)

########################################
#####
# Load data

data <- fread("https://raw.githubusercontent.com/ersan-kucukoglu/Data_Analysis3/main/Assignment2/data/raw/listings.csv?token=GHSAT0AAAAAABQMT6P667CWGOJ6Y2DMYB7OYP36X3Q")


