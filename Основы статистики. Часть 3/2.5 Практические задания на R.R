# Problem 1
library('ggplot2')

exp_data <- read.csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/politeness_data.csv")
str(exp_data)
exp_data$scenario = as.factor(exp_data$scenario)

ggplot(exp_data) +
  geom_boxplot(aes(factor(scenario),frequency, fill = attitude))

# Problem 2 
ggplot(exp_data, aes(frequency, fill = subject)) +
  geom_density(alpha = 0.2) +
  facet_grid(gender~.)

# Problem 3 
library(lme4)
fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), exp_data)

# Problem 4
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), exp_data)

# Problem 5
fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario), exp_data)
