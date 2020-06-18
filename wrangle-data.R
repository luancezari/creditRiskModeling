if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
library(gmodels)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)

#########################
### KNOWNING THE DATA ###
#########################

# Dataset dimension
dim(raw_data) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# First look of data
split(1:ncol(head(raw_data)), sort(rep_len(1:4, ncol(head(raw_data))))) %>%
  map(~select(head(raw_data), .)) %>%
  map(kable, booktabs = T) %>%
  map(kable_styling, latex_options = c("striped", "bordered", "scale_down")) %>%
  walk(print)

# Looking the type of data
raw_data %>%
  summarize_all(~class(.)) %>%
  gather(value = Class) %>%
  arrange(Class) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Searching for missing data
raw_data %>% 
  summarize_all(~sum(is.na(.))) %>% 
  gather(value = NAs) %>%
  mutate(`NAs Proportion` = NAs/nrow(raw_data)) %>%
  arrange(desc(NAs)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Checking if the observations with missing credit score are the same with missing annual income
raw_data %>% summarize(`Are the observations the same?` = identical(which(is.na(`Credit Score`)),
                                                                    which(is.na(`Annual Income`)))) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Looking the credit score distribution
summary(raw_data$`Credit Score`)
raw_data %>% summarize(`number of credit score bigger than 850` = sum(raw_data$`Credit Score` > 850, na.rm = T)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Checking if there's observations with Current Loan Amount bigger than Maximum Open Credit
raw_data %>% 
  summarize(n = sum(`Current Loan Amount` > `Maximum Open Credit`, na.rm = TRUE)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Cheking the distribution of number of credit problems
rbind("Credit Problems" = table(raw_data$`Number of Credit Problems`),
      "proportion" = round(prop.table(table(raw_data$`Number of Credit Problems`)),3)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Cheking the distribution of Tax Liens
rbind("Tax Liens" = table(raw_data$`Tax Liens`),
      "proportion" = round(prop.table(table(raw_data$`Tax Liens`)), 3)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Cheking the distribution of Bankruptcies
rbind("Bankruptcies" = table(raw_data$Bankruptcies),
      "proportion" = round(prop.table(table(raw_data$Bankruptcies)), 3)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Checking how much observations have credit problems and not have bankruptcies historic 
raw_data %>%
  summarize(n = sum(Bankruptcies == 0 & `Number of Credit Problems` > 0, na.rm = T),
            proportion = sum(Bankruptcies == 0 & `Number of Credit Problems` > 0, na.rm = T)/length(na.omit(`Number of Credit Problems`))) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Counting unique data
raw_data %>%
  summarize_all(~n_distinct(., na.rm = TRUE)) %>%
  gather(value = uniques) %>%
  arrange(desc(uniques)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Looking for unique values of categorical columns excluding Loan ID and Customer ID
list(
  "Loan Status" = raw_data %>% pull(`Loan Status`) %>% unique(),
  "Term" = raw_data %>% pull(`Term`) %>% unique(),
  "Home Ownership" = raw_data %>% pull(`Home Ownership`) %>% unique(),
  "Years in current job" = raw_data %>% pull(`Years in current job`) %>% unique(),
  "Purpose" = raw_data %>% pull(`Purpose`) %>% unique())


######################
### WRANGLING DATA ###
######################

wrangled_data <- raw_data %>%
  select(-`Customer ID`,
         -`Months since last delinquent`,
         -Bankruptcies,
         -`Loan ID`) %>%
  mutate(`Credit Score` = ifelse(`Credit Score` > 850,
                                 `Credit Score`/10,
                                 `Credit Score`),
         `Home Ownership` = ifelse(`Home Ownership` == "HaveMortgage",
                                   "Home Mortgage",
                                   `Home Ownership`),
         `Years in current job` = ifelse(`Years in current job` == "n/a",
                                         NA,
                                         `Years in current job`),
         Purpose = ifelse(Purpose == "other",
                          "Other",
                          Purpose),
         Purpose = ifelse(Purpose == "small_business",
                          "Business Loan",
                          Purpose),
         `Historic of Credit Problems` = ifelse(`Number of Credit Problems` == 0, 
                                                "No",
                                                "Yes")) %>%
  filter(!is.na(`Loan Status`),
         !is.na(`Annual Income`),
         raw_data$`Current Loan Amount` < raw_data$`Maximum Open Credit`,
         !is.na(`Years in current job`),
         !is.na(`Tax Liens`)) %>%
  mutate(`Tax Liens` = ifelse(`Tax Liens` == 0,
                              "No",
                              "Yes")) %>%
  distinct() %>%
  select(-`Number of Credit Problems`)

# Converting character columns into factors
wrangled_data[sapply(wrangled_data, is.character)] <- lapply(wrangled_data[sapply(wrangled_data, is.character)], as.factor)

##### Evaluating the wrangled data #####

# Evaluating  class, uniques and NAs of wrangled data
Class <- wrangled_data %>%
  summarize_all(~class(.)) %>%
  gather(value = Class)
nas <- wrangled_data %>% 
  summarize_all(~sum(is.na(.))) %>% 
  gather(value = NAs)
uniques <- wrangled_data %>%
  summarize_all(~n_distinct(.)) %>%
  gather(value = Uniques)
Class %>%
  left_join(uniques, by = "key") %>%
  left_join(nas, by = "key") %>%
  arrange(desc(Class), desc(Uniques)) %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))
rm(Class, uniques, nas)

# Evaluating if there's credit score with decimal places
wrangled_data %>% summarize(`none credit score with decimal` = identical(`Credit Score`,
                                                                         round(`Credit Score`))) %>% gather() %>% 
  kable() %>% 
  kable_styling(latex_options = c("striped", "bordered"))

# Evaluating the dimention of resulting dataset
dim(wrangled_data)

###################################
### CREATING TRAIN AND TEST SET ###
###################################

set.seed(1)
test_index <- createDataPartition(wrangled_data$`Loan Status`, 
                                  times = 1, 
                                  p = 0.2, 
                                  list = FALSE)[,1]

train_set <- wrangled_data[-test_index,]

test_set <- wrangled_data[test_index,]

##### Saving dataframes #####
save(wrangled_data, file="rda/wrangled_data.Rda")
save(train_set, file="rda/train_set.Rda")
save(test_set, file="rda/test_set.Rda")