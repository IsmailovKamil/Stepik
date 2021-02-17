# Problem 2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")

test_data <- as.data.frame(list(X1 = c(9, 12, 10, 9, 10), X2 = c(11, 11, 10, 12, 10), X3 = c(9, 10, 11, 12, 9), X4 = c(12, 11, 10, 9, 9), X5 = c(7, 12, 9, 11, 12)))
var_names = c("X3", "X5", "X4")

centered <- function(test_data, var_names){
  library(dplyr)
  mutate_at(test_data, vars(var_names), funs(. - mean(., rm.na = T)))
}

centered(test_data, var_names)

# Problem 3 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")

get_features <- function(test_data){
  test_data$is_prohibited = as.factor(test_data$is_prohibited)
  fit = glm(is_prohibited ~ ., data = test_data, family = "binomial")
  anova = anova(fit, test = "Chisq")
  if (length(which(anova$`Pr(>Chi)` < 0.05)) == 0) {
  return("Prediction makes no sense")
  } else return(rownames(anova)[which(anova$`Pr(>Chi)` < 0.05)])
}

get_features(test_data)

# Problem 4 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  test_data$is_prohibited = as.factor(test_data$is_prohibited)
  test_data$type = as.factor(test_data$type)
  data_for_predict$passangers = as.factor(data_for_predict$passangers)
  
  fit = glm(is_prohibited ~ ., data = test_data, family = "binomial")
  pred = predict(fit, newdata = data_for_predict, type = "response")
  return(as.character(data_for_predict$passangers[which(pred == max(pred))]))
}

most_suspicious(test_data, data_for_predict)

# Problem 5 
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

num = test[which(sapply(test, is.numeric))]
shapiro = sapply(num, function(x) shapiro.test(x)$p.value)

normality_test <- function(dataset){
  num = dataset[which(sapply(dataset, is.numeric))]
  sapply(num, function(x) shapiro.test(x)$p.value)
}

normality_test(iris)

# Problem 6 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
test_data <- as.data.frame(list(x = c(0.22, 0.04, -0.66, -0.39, 0.08, 0.3, -0.53, 0.09, -0.37, -0.63, -0.15, -1.29, 1.24, 0.46, -1.1, -1.42, 0.07, 0.43, -0.65, 1.43, -0.32, 1.38, 0.09, 0.21, 0.36, -0.47, -1.47, 0.53, 0.43, 0.37), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data <- as.data.frame(list(x = c(-1.38, 0.63, 0.3, -1.57, -0.22, 0.43, -0.86, -1.43, 0.2, 0.65, -1.14, -0.63, -0.11, -0.59, -0.67, -0.99, -1.05, -0.04, -1.02, -1.1, -0.11, 0.58, 0.15, -1.42, -1.63, -1.54, -0.03, 0.42, 2.98, 0.89), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))

smart_anova <- function(test_data){
  library(dplyr)
  test_data$y = as.factor(test_data$y)
  norm = summarize(group_by(test_data, y),
          shapiro = shapiro.test(x)$p.value)
  bartlett = bartlett.test(x ~ y, data = test_data)$p.value
  if (min(norm[2]) > 0.05 & bartlett > 0.05) {
  a = aov(x ~ y, data = test_data)
  ANOVA = summary(a)[[1]]$'Pr(>F)'[1]
  names(ANOVA) = "ANOVA"
  print(ANOVA)
} else {
  k = kruskal.test(x ~ y, test_data)
  KW = k$p.value
  names(KW) = "KW"
  print(KW)
}
}        

smart_anova(test_data)

# Problem 7 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")

normality_by <- function(test){
  names(test) = c("x", "y", "z")
  gr = group_by(test, y, z)
  summarise(gr, p_value = shapiro.test(x)$p.value)
}

normality_by(test_data)

# Problem 8 
library(ggplot2)

ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.2)
