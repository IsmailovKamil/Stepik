# Step 3
library(dplyr)

my_data = data_frame(x = rnorm(10000),
                     y = rnorm(10000),
                     f = factor(rep(1:2,5000)))

my.data = data.frame(x = rnorm(10000),
                     y = rnorm(10000),
                     f = factor(rep(1:2,5000)))

library(ggplot2)

diamonds = as_data_frame(diamonds)
diamonds

# Step 4 
my_data_2 = data_frame("My Var" = rnorm(10))
my_data_2$`My Var`

my_data_3 = data_frame(x = rnorm(10), y = abs(x))

# Step 5
select(diamonds, 1,2,3)
select(diamonds, starts_with("c"))
select(diamonds, ends_with("t"))
select(diamonds, contains("t"))

# Step 6
slice(diamonds,2:10)
slice(diamonds,c(1,4,5))

# Step 7
filter(diamonds, carat > 0.3, color == "J")
diamonds[diamonds$carat > 0.3 & diamonds$color == "J",]
subset(diamonds, carat > 0.3 & color == "J")

# Step 8
arrange(diamonds, price, depth)
diamonds[order(diamonds$price, diamonds$depth),]

arrange(diamonds,desc(price))

#Step 9
rename(diamonds, new_cut = cut, new_carat = carat)
names(mtcars)[1] = "new_mpg"

m = mutate(diamonds, sqrt_price = sqrt(price))
plot(m$sqrt_price, m$carat)

m = mutate(diamonds, sqrt_price = sqrt(price),
           log_carat = log(carat))

str(mutate(mtcars, am = factor(am), vs = factor(vs)))

# Problem 1 
slice(diamonds,which(seq(nrow(diamonds))%%2 != 0))

# Problem 2 
mtcars %>%
  select(mpg, hp, am, vs) %>%
    filter(mpg > 14, hp > 100) %>%
      arrange(desc(mpg)) %>%
        slice(1:10) %>%
         rename("Miles per gallon" = mpg, "Gross horsepower" = hp)
            
