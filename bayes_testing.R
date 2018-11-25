# TAKES CTR DATA FROM GOOGLE SEARCH CONSOLE
# YOU CAN ALSO USE CONVERSION RATE DATA AS WELL
# COMPUTES PROBABILITY STATISTICS BETWEEN A GIVEN CONTROL AND EXPERIMENT GROUP
# BASED ON ONE MILLION PROBABILITY DISTRIBUTION SAMPLES

# LOAD PACKAGES AND PARSE DATA
library(bayesAB)
library(tidyverse)
library(scales) 
library(RColorBrewer)
library(lubridate)

x <- read_csv("your_csv_file.csv")

control <- x %>%
  filter(segment == 'landing page, experiment type or sample population') 

experiment <- x %>%
  filter(keyword == 'landing page, experiment type or sample population') 

# PLOT DENSITY OF CONTROL VS EXPERIMENT DATA
ggplot() + 
  geom_density(data = control,
               aes(CTR), 
               alpha = 0.3,
               fill = 'salmon',
               color = 'salmon') +
  geom_density(data = experiment,
               aes(CTR),
               alpha = 0.3,
               fill = 'steelblue',
               color = 'steelblue') +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = control, aes(xintercept = mean(CTR)), color = 'salmon', linetype = 'dashed') +
  geom_vline(data = experiment, aes(xintercept = mean(CTR)), color = 'steelblue', linetype = 'dashed') +
  theme_bw(base_size = 15) +
  labs(x = "",
       y = "")

# CREATE DATA FRAMES NECESSARY FOR BAYESAB PACKAGE
control_data <- control %>% 
  summarize(impressions = sum(Impressions),
            ctr = sum(Clicks) / impressions)
experiment_data <- experiment %>%
  summarize(impressions = sum(Impressions),
            ctr = sum(Clicks) / impressions)

A_data <- rbinom(n = control_data$impressions, p = control_data$ctr, size = 1)
B_data <- rbinom(n = experiment_data$impressions, p = experiment_data$ctr, size = 1)

# OUTPUT BAYESIAN AB TEST STATISTICS
test <- bayesTest(A_data, B_data, 
                  priors = c('alpha' = 5, 'beta' = 5), 
                  n_samples = 1e5, 
                  distribution = 'bernoulli')

