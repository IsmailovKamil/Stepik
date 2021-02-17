fit1 = lm(mpg ~ hp, mtcars)
fit2 = lm(mpg ~ I(-hp^-0.7), mtcars)

summary(fit1)
summary(fit2)

qplot(x = log(hp), y = log(mpg), data = mtcars)

head(swiss)

fit_1 = lm(Fertility ~ ., swiss)
summary(fit_1)

cor.test(~ Fertility + Examination, swiss)

library(dplyr)
library(DAAG)
library(car)

vif(fit_1)

fit_2 = lm(Fertility ~., select(swiss, -Examination))
summary(fit_2)

vif(fit_2)

fit_3 = lm(Sepal.Length ~ ., select(iris, -Species))
summary(fit_3)           
vif(fit_3)

# Problem 1 
hetero_test <-  function(test_data){
  x = names(test_data)[1]
  y = names(test_data)[-1]
  fit = lm(as.formula(paste(x, paste(y, collapse=" + "), sep=" ~ ")), test_data)
  fit2 = lm((fit$residuals)^2 ~ .,test_data[,-1])
  return(summary(fit2)$r.squared)
}

hetero_test(mtcars)

# Problem 2 

x = names(test_data)[1]
y = names(test_data)[-1]
fit = lm(as.formula(paste(x, paste(y, collapse=" + "), sep=" ~ ")), test_data)
summary(fit)

fit2 = lm(mtcars$"cyl" ~., mtcars[,-1])

VIF <-  function(test_data){
  y = names(test_data)[-1]
  z = lapply(y, function(x) {1/(1-summary(lm(as.formula(paste(x, paste(y[which(y!=x)], collapse=" + "), sep=" ~ ")), test_data[,-1]))$r.squared)})
  names(z) = y
  return(sapply(z, print))
}

VIF(mtcars)

# Problem 3 
set.seed(42)
test_data <- data.frame(y = rnorm(30, 5), x1 = rnorm(30, 5))
test_data$x2 <- test_data$x1^2
VIF(test_data)

# Problem 4

