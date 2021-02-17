# Step 1 
library(dplyr)
d = as_data_frame(matrix(rnorm(30), ncol = 5))
d

mutate_all(d, funs(abs))

as.data.frame(sapply(d, function(x) ifelse(x < 0 ,0,x)))
sapply(d, function(x) abs(x))

mutate_all(d, funs(. * 2))
mutate_all(d, funs(ifelse(. < 0 ,0, .)))

col_1 = d$V1
ifelse(col_1 < 0,0,col_1 )

# Problem 1 
all_to_factor = function (x) {
  as.data.frame(mutate_all(x, funs(factor)))
}

# Problem 2 
test_data <- as.data.frame(list(V1 = c(1.5,-0.1,2.5,-0.3,-0.8), V2 = c(-0.5, 1.2, -0.6, 0.8, -0.6), V3 = c(-0.9, -1.4, 1.8, 0.2, -1.4), V4 = c(-1.4, 0, -1, -0.2, -1.1), V5 = c("B", "B", "B", "B", "B")))
head(iris)
log_transform <- function(test_data){
  x = test_data[sapply(test_data, is.numeric)]
  y = test_data[sapply(test_data, function (y) !is.numeric(y))]
  z = (mutate_all(x, funs(log((.-min(.))/(max(.)-min(.)) + 1))))
  return(cbind(z,y))
}

# Alternative solution 
log_transform <- function(test_data){      
  rescaling <- function(x){      
    log((x - min(x)) / (max(x) - min(x)) + 1)}      
  num_var <- sapply(test_data, is.numeric)      
  test_data[num_var] <- mutate_each(test_data[num_var], funs(rescaling))      
  return(test_data)}

# Step 6 
library(ggplot2)
diamonds = as_data_frame(diamonds)

gr_diamonds = group_by(diamonds,cut)
slice(gr_diamonds,1)

# step 9 
summarise(gr_diamonds,
          number = n(),
          mean_price = mean(price),
          mean_x = mean(x),
          median_y = median(y), 
          min_y = min(y))

# Problem 3 
test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")

descriptive_stats <- function (dataset){
  gr_data = group_by(dataset, gender, country)
  summarise(gr_data,
            n = n(),
            mean = mean(salary, na.rm = T),
            sd = sd(salary, na.rm = T),
            median = median(salary, na.rm = T),
            first_quartile = quantile(salary, 0.25, na.rm = T),
            third_quartile = quantile(salary, 0.75, na.rm = T),
            na_values = sum(is.na(salary))
  )
}

descriptive_stats(test_data)

# Problem 4 
data = mtcars[1:4]
factors = c(1,3)
to_factors <- function(data, factors){
  data = mutate_at(data, vars(factors), funs(factor(ifelse(.>mean(.), 1, 0))))
  return(data)
}
test_data <- as.data.frame(list(V1 = c(9, 9, 10, 10, 10, 10), V2 = c(9, 8, 10, 9, 12, 10), V3 = c(12, 10, 9, 10, 10, 9), V4 = c(9, 10, 9, 9, 8, 9), V5 = c(9, 10, 10, 8, 10, 9), V6 = c(10, 10, 11, 10, 11, 10), V7 = c(10, 12, 10, 10, 9, 8), V8 = c(9, 9, 7, 10, 10, 10), V9 = c(10, 8, 9, 10, 9, 10), V10 = c(9, 10, 12, 11, 9, 11)))

# Problem 5
high_price = diamonds %>%
                select(color, price) %>%
                group_by(color) %>%
                arrange(desc(price), .by_group = T) %>%
                slice(1:10)
