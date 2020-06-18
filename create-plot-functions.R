if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(anomalize)) install.packages("anomalize", repos = "http://cran.us.r-project.org")
library(anomalize)

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