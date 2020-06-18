##########################
### REQUIRED LIBRARIES ###
##########################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
library(gmodels)
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
library(ggcorrplot)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)
if(!require(anomalize)) install.packages("anomalize", repos = "http://cran.us.r-project.org")
library(anomalize)
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(ggpubr)
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
library(GGally)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)

######################
### IMPORTING DATA ###
######################

raw_data <- read_csv("data/credit_train.csv")


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

###############################
### CREATING PLOT FUNCTIONS ###
###############################

# create categorical distribution plot function
barplt <- function(variable){
  variable <- enquo(variable)
  train_set %>%
    ggplot(aes(x = !!variable, fill = `Loan Status`)) +
    geom_bar(position = "stack") +
    labs(x = "",
         y = "",
         title = "") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
}

# create categorical porportion plot function
barplt_prop <- function(variable){
  dta = data.frame(yi = 0.727)
  variable <- enquo(variable)
  train_set %>%
    ggplot(aes(x = !!variable, fill = `Loan Status`)) +
    geom_bar(position = "fill") +
    labs(x = "",
         y = "",
         title = "") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    geom_hline(data = dta,
               aes(yintercept = yi,
                   linetype = factor(yi)), size = 1) +
    scale_linetype_manual(name = "Overall proportion", 
                          values = "dashed", 
                          labels = "") +
    guides(fill = guide_legend(override.aes = list(linetype = "blank")))
}

# Create density plot without outliers function
dty_plot <- function(variable){
  variable <- enquo(variable)
  train_set %>%
    anomalize(!!variable, method = "iqr") %>%
    filter(anomaly == "No") %>%
    ggplot(aes(!!variable,
               fill = `Loan Status`)) +
    geom_density(alpha = 0.5) +
    labs(x = "",
         y = "",
         title = variable) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}


##############################
### EXPLORING TRAINING SET ###
##############################

##### Exploring outcome #####
# Proportion of paid x default
CrossTable(train_set$`Loan Status`)

##### Exploring categorical features #####
# Purpose feature
annotate_figure(ggarrange(
  train_set %>%
    mutate(Purpose = forcats::fct_reorder(.f = Purpose,
                                          .x = `Loan Status`,
                                          .fun = function(.x) mean(.x == "Charged Off"))) %>%
    ggplot(aes(x = Purpose, 
               fill = `Loan Status`)) +
    geom_bar(position = "dodge") +
    labs(x = "",
         y = "",
         title = "") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none"),
  train_set %>% 
    mutate(Purpose = forcats::fct_reorder(.f = Purpose,
                                          .x = `Loan Status`,
                                          .fun = function(.x) mean(.x == "Charged Off"))) %>%
    ggplot(aes(x = Purpose, 
               fill = `Loan Status`)) +
    geom_bar(position = "fill") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    labs(x = "",
         y = "",
         title = "") +
    geom_hline(aes(yintercept = 0.727,
                   linetype = "Fully Paid"), size = 1) +
    scale_linetype_manual(name = "Overall proportion", 
                          values = c(1,1)),
  nrow = 2, common.legend = TRUE, legend = "bottom", align = "v"),
  top = "Purpose")

# Years in current job feature
annotate_figure(ggarrange(
  train_set %>%
    mutate(`Years in current job` = factor(`Years in current job`,
                                           levels = c("< 1 year", "1 year", "2 years", "3 years", 
                                                      "4 years", "5 years", "6 years", "7 years",
                                                      "8 years","9 years", "10+ years"))) %>%
    ggplot(aes(x = `Years in current job`, 
               fill = `Loan Status`)) +
    geom_bar(position = "dodge") +
    labs(x = "",
         y = "",
         title = "") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none"),
  train_set %>%
    mutate(`Years in current job` = factor(`Years in current job`,
                                           levels = c("< 1 year", "1 year", "2 years", "3 years", 
                                                      "4 years", "5 years", "6 years", "7 years",
                                                      "8 years","9 years", "10+ years"))) %>%
    ggplot(aes(x = `Years in current job`, 
               fill = `Loan Status`)) +
    geom_bar(position = "fill") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    labs(x = "",
         y = "",
         title = "") +
    geom_hline(aes(yintercept = 0.727,
                   linetype = "Fully Paid"), size = 1) +
    scale_linetype_manual(name = "Overall proportion", 
                          values = c(1,1)),
  nrow = 2, common.legend = TRUE, legend = "bottom", align = "v"),
  top = "Years in current job")

# Term feature
annotate_figure(ggarrange(barplt(Term),
                          barplt_prop(Term),
                          ncol = 2, common.legend = TRUE, legend = "bottom"),
                top = "Term")

# Tax Liens feature
annotate_figure(ggarrange(barplt(`Tax Liens`),
                          barplt_prop(`Tax Liens`),
                          ncol = 2, common.legend = TRUE, legend = "bottom"),
                top = "Tax Liens")

# Historic of Credit Problems feature
annotate_figure(ggarrange(barplt(`Historic of Credit Problems`),
                          barplt_prop(`Historic of Credit Problems`),
                          ncol = 2, common.legend = TRUE, legend = "bottom"),
                top = "Historic of Credit Problems")

# Home Ownership feature
annotate_figure(ggarrange(barplt(`Home Ownership`),
                          barplt_prop(`Home Ownership`),
                          ncol = 2, common.legend = TRUE, legend = "bottom"),
                top = "Home Ownership")

##### Exploring continuous features #####

# Plot density of continuous features without outliers
ggarrange(
  dty_plot(`Current Credit Balance`),
  dty_plot(`Monthly Debt`),
  dty_plot(`Annual Income`),
  dty_plot(`Years of Credit History`),
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggarrange(
  dty_plot(`Number of Open Accounts`),
  dty_plot(`Maximum Open Credit`),
  dty_plot(`Credit Score`),
  dty_plot(`Current Loan Amount`),
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

##### Exploring the relation between some features #####

# Cross-table between historic of credit problems and tax liens
CrossTable(train_set$`Historic of Credit Problems`, 
           train_set$`Tax Liens`,
           prop.chisq = F)

##### Correlation matrix of continuous variables #####
numeric_train_set <- train_set %>%
  select(-`Loan Status`,
         -`Term`,
         -`Years in current job`,
         -`Home Ownership`,
         -Purpose,
         -`Tax Liens`,
         -`Historic of Credit Problems`)
trainSet_cor <- cor(numeric_train_set)
ggcorrplot(trainSet_cor,
           hc.order = TRUE,
           lab = TRUE)
rm(numeric_train_set, trainSet_cor)

#################
### MODELLING ###
#################

# Adapting `Loan Status` and Set Fully_Paid as first factor level

train_set <- train_set %>% 
  mutate(`Loan Status` = factor(`Loan Status`, 
                                levels = c("Fully Paid", "Charged Off")))

test_set <- test_set %>% 
  mutate(`Loan Status` = factor(`Loan Status`, 
                                levels = c("Fully Paid", "Charged Off")))

levels(train_set$`Loan Status`) <- c("Fully_Paid", "Charged_Off")
levels(test_set$`Loan Status`) <- c("Fully_Paid", "Charged_Off")

# Create balanced accuracy function
baSummary <- function(data, lev = NULL, model = NULL){
  out <- (sensitivity(data$pred, data$obs)+specificity(data$pred, data$obs))/2
  c(balancedAccuracy = out)
}

# Create train control
train.control <- trainControl(method = "cv", 
                              number = 5, 
                              p = .8,
                              classProbs = TRUE,
                              summaryFunction = baSummary)

##### LOGISTIC REGRESSION MODEL #####

# Training model
glm_fit <- train(`Loan Status` ~ .,
                 data = train_set, 
                 method = "glm",
                 metric = "balancedAccuracy",
                 family = "binomial",
                 trControl = train.control)

# Summary
summary(glm_fit)

# Evaluating model
confusionMatrix(predict(glm_fit, test_set), test_set$`Loan Status`)

##### LOGISTIC REGRESSION MODEL 2 #####

# Training model
glm_fit2 <- train(`Loan Status` ~ `Current Loan Amount` + `Term` + `Credit Score` + 
                    `Annual Income` + `Purpose` + `Monthly Debt` + 
                    `Number of Open Accounts` + `Home Ownership`,
                  data = train_set, 
                  method = "glm",
                  metric = "balancedAccuracy",
                  family = "binomial",
                  trControl = train.control)

# Summary
summary(glm_fit2)

# Evaluating model
confusionMatrix(predict(glm_fit2, test_set), test_set$`Loan Status`)

##### RANDOM FOREST MODEL #####

# Training model
set.seed(1)
rf_fit <- train(`Loan Status` ~ .,
                data = train_set, 
                method = "rf",
                metric = "balancedAccuracy",
                trControl = train.control,
                tuneGrid = data.frame(mtry = seq(500, 1000, 250)),
                importance = TRUE)

# Evaluating model
confusionMatrix(predict(rf_fit, test_set), test_set$`Loan Status`)