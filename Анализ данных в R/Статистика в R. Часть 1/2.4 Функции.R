my_calc <- function(x,y){
  s <- x + y
  d <- x - y
  return(c(s, d))
}

result = my_calc(10,15)
result

distr1 = rnorm(100)
distr1[1:30] = NA

distr1[is.na(distr1)] = mean(distr1, na.rm = T)
hist(distr1)

d1 <- rnorm(2000)
d2 <- runif(2000)

d1[1:10] = NA
d2[1:10] = NA

d1 = my_na_rm(d1)
d2 = my_na_rm(d2)

source("my_na_rm.R")

# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе. На вход функция получает 
# числовой вектор с пропущенными значениями. Функция возвращает новый вектор с номерами позиций пропущенных значений.
x = c(1, 2, 3, NA, NA)
NA.position <- function(x){
  y = c()
  for (i in 1:length(x)) {
    if (is.na(x[i]) == TRUE) {
      y = c(y,i)
    }
  }
  return(y)
}
NA.position(x)

# 2 method
NA.position_2 <- function(x){    
  which(is.na(x))}

NA.position_2(x)

# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.
# На вход функция  NA.counter должна принимать один аргумент - числовой вектор. 
# Функция должна возвращать количество пропущенных значений.
NA.counter <- function(x){
  y = 0
  for (i in 1:length(x)) {
    if (is.na(x[i]) == TRUE) {
      y = y + 1
    }
  }
  return(y)
}

# 2 method
NA.counter_2 <- function(x){    
  return(sum(is.na(x)))}

# combine all files
dir(pattern = '.csv')

grants = data.frame()

for (i in dir(pattern = '*.csv')) {
  temp_df = read.csv(i)
  grants = rbind(grants, temp_df)
}

# function combining all files in current directory
read_data = function(){
  df = data.frame()
  number = 0
  for (i in dir(pattern = '*.csv')) {
    temp_df = read.csv(i)
    df = rbind(df, temp_df)
    number = number + 1
  }
  print(paste(as.character(number), 'files were comlined'))
  return(df)
}

grants2 = read_data()

# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными, 
# положительными и отрицательными значениями и возвращает сумму положительных элементов вектора.

filtered.sum <- function(x){
  y = 0
  for (i in 1:length(x)){
    if (x[i] > 0 & is.na(x[i]) == FALSE) {
      y = y + x[i]
    }
  }
    return(y)
}

# Method 2 
filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}

# Напишите функцию outliers.rm, которая находит и удаляет выбросы.
outliers.rm <- function(x){
  o = c()
  quan_25 = quantile(x, 0.25) - 1.5 * IQR(x)
  quan_75 = quantile(x, 0.75) + 1.5 * IQR(x)
  for (i in x) {
    if (i < quan_75 & i > quan_25) {
      o = c(o,i)
    }
  }
  return(o)
}

# method 2
outliers.rm_2 <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])}
