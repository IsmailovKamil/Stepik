# Problem 1 
smart_test <-  function(x){
  x = table(x)
  if (all(x > 5)) {
    y = c(as.numeric(chisq.test(x)$statistic), as.numeric(chisq.test(x)$parameter), chisq.test(x)$p.value)
    return(y) 
  } else return(fisher.test(x)$p.value)
}

# Problem 2 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
test_data$V4 = test_data$V3
names(which.min((apply(sapply(test_data, table), 2,function(x) chisq.test(x)$p.value))))

x = (apply(sapply(test_data, table), 2,function(x) chisq.test(x)$p.value))
most_significant <-  function(test_data){
  x = (apply(sapply(test_data, table), 2,function(x) chisq.test(x)$p.value))
  return(names(x[x == min(x)]))
}

most_significant(test_data)

names(x[x == min(x)])

# Problem 3

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")

test_data <- transform(test_data, x = factor(x), y = factor(y))
get_coefficients <- function(dataset){
fit = glm(y ~ x, dataset, family = "binomial")
return(exp(fit$coefficients))
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
names(test_data)

# Problem 4
y = apply(iris[,1:4], 2, function(x) ifelse(x > mean(x), 1, 0))
iris$important_cases = as.factor(apply(y, 1, function(x) ifelse(sum(x) >= 3, "Yes", "No")))

str(iris$imp)
table(iris$imp)

# Problem 5
x <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

test_data <- as.data.frame(list(V1 = c(23, 18, 16, 15, 16), V2 = c(19, 18, 25, 19, 28), V3 = c(22, 20, 17, 25, 23), V4 = c(23, 9, 20, 21, 14), V5 = c(19, 16, 18, 20, 28), V6 = c(12, 17, 12, 25, 23)))

get_important_cases <- function(x){
y = apply(x, 2, function(x) ifelse(x > mean(x), 1, 0))
x$important_cases = factor(1,0,levels = c("Yes", "No"), labels = c("Yes", "No"))
x$important_cases = as.factor(apply(y, 1, function(x) ifelse(sum(x) > length(x) - sum(x), "Yes", "No")))
return(x)
}

str(get_important_cases(test_data))

# Problem 6
v1 <- c(1, 2, 3, 3, 3, 4, 5)
v2 <- c(1, 1, 1, 2, 3, 3, 3)
stat_mode <- function(x){
  x = table(x)
  as.numeric(names(which(max(x) == x)))
}

stat_mode(v2)

# Problem 7
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
max_resid <- function(x){
  x = table(x)
  c = chisq.test(x)$stdres
  row = which(c == max(c),arr.ind = TRUE)[1]
  col = which(c == max(c),arr.ind = TRUE)[2]
  return(c(rownames(c)[row], colnames(c)[col]))
}

max_resid(test_data)

# Problem 8
install.packages("ggplot2") # если у вас не установлен пакет
library("ggplot2")
# теперь данные diamonds доступны для работы
str(diamonds)

ggplot(diamonds, aes(color) ) +
  geom_bar(aes(fill = cut), position = "dodge") 
