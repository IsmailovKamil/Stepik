# step 1 lapply 
apply(array, margin, ...)

lapply(list, function)

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
str(my_list)

lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)

sapply(my_list, range, na.rm = T, simplify = F)

# Problem 1 
d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

lapply(d,function(x) sum(x[!is.na(x) & x > 0]))
positive_sum <-  function(test_data){
  lapply(test_data,function(x) sum(x[!is.na(x) & x > 0]))
}

# step 2

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"  


sapply(cars, function(x) grepl(x, car))
lapply(cars, function(x) grepl(x, car))

iris_num <- iris[sapply(iris, is.numeric)]
sapply(iris[1:4], sd)

# step 3 by tapply
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))

by(iris[1:4], iris$Species, 
   function(x) sapply(x, 
                      function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


# step 4 vapply, 

vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))

rep(1, 3)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)

m <- matrix(rnorm(100 * 200), nrow = 100)
m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")
rownames(m)= m_names[[1]]
colnames(m) = m_names[[2]]

# Problem 1 
test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), 
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

names = c('HPS1', 'GOT1')
my_names <- function (dataset, names){
  dataset[sort(as.numeric(sapply(lapply(names, function (x) grepl(x, dataset[[1]])), which))),]
}

my_names(test_data, names)

# Problem 2

ToothGrowth$dose <- factor(ToothGrowth$dose)
x = ToothGrowth
gr = group_by(x, x[sapply(x, is.factor)])
summarise(gr,
          mean = mean(len),
          sd = sd(len))

aggregate(, mean)
# Problem 3
head(swiss)
test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")

smart_lm <- function(dataset){
  z = dataset[-1]
  x = z[which(as.numeric(sapply(z, function (x) shapiro.test(x)$p.value)) > 0.05)]
  if (length(x) != 0) {
  y = as.vector(lm(as.formula(paste("dataset[,1]~", paste(x, collapse="+"))))$coefficients)
  return(y)
  } else print("There are no normal variables in the data")
}

# Problem 4
one_sample_t <- function(df, general_mean){
  x = df[sapply(df, is.numeric)]
  y = lapply(x, function(x) t.test(x,mu = general_mean))
  lapply(y, function(x) c(x$statistic, x$parameter, x$p.value))
}

# Problem 5
get_p_value <- function(test_list){
  lapply(test_list, function (x) x$p.value)
}

