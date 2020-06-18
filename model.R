if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)

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