### NOTE: Before run this code run create-plot-functions.R ###

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