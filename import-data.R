if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

raw_data <- read_csv("data/credit_train.csv")
