## Step 1
?swiss

swiss = data.frame(swiss)

hist(swiss$Fertility)

# numeric predictor
fit = lm(Fertility ~ Examination + Catholic, swiss)
summary(fit)

# with interaction 
fit2 = lm(Fertility ~ Examination * Catholic, swiss)
summary(fit2)

# confident intervals
confint(fit2)

# Problem 1
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

fill_na = function(test_data) {
  fit3 = lm(y ~ x_1 + x_2, test_data )
  test_data$y_full = predict(fit3,test_data)
  test_data$y_full <- ifelse(is.na(test_data$y), test_data$y_full, test_data$y)
  return(test_data)
}

fill_na(test_data)

# Problem 2
library("MASS")
df = subset(x = mtcars, select = c(wt, mpg, disp, drat, hp))
stepAIC(model, trace = FALSE)
model <- lm(wt ~ mpg + disp + hp, df)
summary(model)

# Problem 3
summary(lm(rating ~ complaints*critical, attitude))

## Step 8
hist(swiss$Catholic, col = "red")

swiss$religious = ifelse(swiss$Catholic > 60, "Lots", "Few")
swiss$relisious = as.factor(swiss$religious)  

fit3 = lm(Fertility ~ Examination + religious, data = swiss)  
summary(fit3)  
  
fit4 = lm(Fertility ~ religious*Examination, data = swiss)  
summary(fit4)  

## Step 9
library("ggplot2")

ggplot(swiss, aes(Examination,Fertility, col = religious)) +
  geom_point() +
  geom_smooth(method = "lm")

## Step 11

fit5 = lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

## Step 12
# Памятка по интерпретации результатов регрессионного анализа 
# с категориальными и непрерывными переменными

# Модель для примера:
  
#  DV ~ IV_numeric * IV_categorical

#IV_categorical - фактор с двумя уровнями (Level1 и Level2)

#Коэффициенты:
  
#  Intercept — предсказанное значение DV для первого уровня IV_categorical 
#  с учётом того, что IV_numeric равна нулю.

# IV_numeric — насколько изменяется предсказанное значение DV при увеличении 
# IV_numeric на одну единицу в группе, соответствующей первому уровню IV_categorical

# IV_categoricalLevel2 — насколько изменяется предсказанное значение DV 
# при переходе от первого уровня IV_categorical ко второму уровню. 
# С учётом того, что IV_numeric равна нулю.

# IV_numeric:IV_categoricalLevel2 — насколько сильнее (или слабее) изменяется 
# предсказанное значение DV при увеличении IV_numeric на одну единицу в группе, 
# соответствующей второму уровню IV_categorical, по сравнению с первым уровнем.

# Problem 1,2
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

fit6 = lm(mpg ~ am*wt, data = mtcars)
summary(fit6)

ggplot(mtcars, aes(wt,mpg, color = am)) +
  geom_point() +
  geom_smooth(method = "lm")

# Problem 3 
my_plot <- ggplot(mtcars, aes(wt,mpg, col = "am")) + geom_smooth(method = "lm")

                  