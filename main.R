# This is test trading strategy on the S&P 500 stocks
library(rstudioapi) # Provides us with basic utilities like document path and setting directory and so on.
library(tidyquant) # Provides us with s&p 500 symbols and s&p 500 data
library(dplyr)
library(TTR)
library(randomForest)
library(keras)
library(tensorflow)
library(ranger)

# Setting up the work environment
curr_dir_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_dir_path))
rm(list = ls())
cat("\014")
print("Cleared all variables")
# Environment is set and all global variables are cleared.


source("download_data.R")

source("indicators.R")

source("signals.R")

source("news_sentiment.R")

source("run_strategy.R")
