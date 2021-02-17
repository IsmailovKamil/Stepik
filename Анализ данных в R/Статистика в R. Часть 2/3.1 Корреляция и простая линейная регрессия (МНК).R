# Step 1
df = mtcars
o = (cor(numeric_df))
colnames(o[1])
fit = cor.test(x = df$mpg, y= df$hp)

cor.test(~mpg + hp, df)

str(fit)

fit$p.value

# Step 2
plot(df$mpg, df$hp)

library("ggplot2")

ggplot(df, aes(mpg,hp, col = factor(cyl))) +
  geom_point()

# Step 3
df_numeric = df[,c(1,3:7)]

pairs(df_numeric)

library("psych")
fit2 = corr.test(df)
str(fit2$r)
fit2$p

corr.calc <- function(x){
  y = cor(x)
  z = corr.test(x)
  return(c(y[2],z$p[2]))
}

corr.calc(iris[,1:2])

# Напишите функцию filtered.cor которая на вход получает data.frame с  произвольным 
# количеством переменных (как количественными, так и любых других типов), рассчитывает 
# коэффициенты корреляции Пирсона между всеми парами количественных переменных и 
# возвращает наибольшее по модулю значение коэффициента корреляции. 

filtered.cor <- function(x){
  numeric_df = data.frame()
  y = c()
  for (i in 1:length(names(x))) {
    if (class(x[,i]) == "numeric"){
      y = c(y,i)
      numeric_df = x[,y]
    }
  } 
  library("psych")
  corr = corr.test(numeric_df)
  o = as.data.frame(as.table(corr$r))
  z = c()
  for (i in 1:nrow(o)) {
    if (o[i,1] != o[i,2]) {
      z = c(z,o[i,3])}
  }
  if (abs(min(z)) > max(z)){
    return(min(z))
  } else return(max(z))
}

# method 2 
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}

# Напишите функцию smart_cor, которая получает на вход dataframe с двумя 
# количественными переменными. Проверьте с помощью теста Шапиро-Уилка, 
# что данные в обеих переменных принадлежат нормальному распределению.

x  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor <- function(x){
  library("psych")
if (shapiro.test(x[,1])$p.value < 0.05 | shapiro.test(x[,2])$p.value < 0.05) {
  return(corr.test(x, method = "spearman")$r[2])
} else return(corr.test(x, method = "pearson")$r[2])
}
smart_cor(x)

# Step 8
df = mtcars
df_numeric = df[,c(1,3:7)]

fit = lm(mpg ~ hp, df)
summary(fit)

ggplot(df, aes(hp,mpg)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(.~cyl)

# Step 9
fitted_values_mpg = data.frame(mpg = df$mpg,
                               fitted = fit$fitted.values)

# Step 10
new_hp = data.frame(hp = c(100,150,129,300))
new_hp$mpg = predict(fit, new_hp)

# Step 11
my_df = mtcars
my_df$cyl = factor(my_df$cyl, labels = c("four", "six", "eight"))

fit_2 = lm(mpg ~ cyl, my_df)
summary(fit_2)

aggregate(mpg ~ cyl, my_df, mean)

ggplot(my_df, aes(cyl,mpg)) +
  geom_point()

# Problem 1
my_data = read.table("dataset_11508_12.txt")
fit_3 = lm(V1 ~ V2, my_data)
summary(fit_3)

# Problem 2
my_df_3 = subset(diamonds, cut == "Ideal" & carat == 0.46)
fit_4 = lm(price ~ depth, my_df_3)
fit_coef = fit_4$coefficients

# Problem 3 
# Если две переменные значимо коррелируют (p - уровень значимости 
# для коэффициента корреляции Пирсона меньше 0.05), то функция строит 
# регрессионную модель, где первая переменная - зависимая, вторая - независимая. 
# Затем создает в dataframe новую переменную с назанием fit, где сохраняет 
# предсказанные моделью значения зависимой переменной. В результате функция 
# должна возвращать исходный dataframe с добавленной новой переменной fit.
regr.calc = function(my_df) {
library("psych")
cor = corr.test(my_df)$p
if (cor[2] < 0.05) {
  model = lm(my_df[,1] ~ my_df[,2], my_df)
  my_df$fit = model$fitted.values
  return(my_df)
} else return("There is no sense in prediction")
}

# Problem 4
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

## Памятка
cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 

cor.test(~ mpg + disp, mtcars) # запись через формулу


cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 

cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 

cor(iris[, -5]) # построение корреляционной матрицы

fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 

fit$coefficients # коэффициенты регрессии 

fit$fitted.values # предсказанные значения зависимой переменной

# При наличии одинаковых значений в переменных расчет непараметрических корреляций 
# будет сопровождаться предупреждением о невозможности рассчитать точное значение p - value.

# Если в ваших данных есть одинаковые наблюдения, но вы хотите рассчитать непараметрическую 
# корреляцию, используйте функцию spearman_test  из пакета coin

library("coin")
spearman_test(~ mpg + disp, mtcars)

# Обратите внимание на различия в графиках. То что в первом aes() будет распространяться на все слои. 
# А то, что в aes() конкретного geom - только на него.
ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))
