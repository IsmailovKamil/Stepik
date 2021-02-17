# Step 2
library(data.table)
products = fread("products.csv")
products[1:10,]

products[price > 10000] 

products[(price > 10000) &
           (brand %in% c("Epson", "Apple"))]

# Step 3 
products[available == T]
product[3,]

# Step 4 
products[, list(name,
                price.1k = price/1000)]

order(products$price, decreasing = T)
products[order(price, decreasing = T)]
head(products[order(price,decreasing = T), 
         list(name, price.1k = paste0(price / 1000, " тыс.руб"))],5 )

# Step 5 
head(products[order(price, decreasing = T),
         list(price.1k = paste0(price / 1000, "тыс.руб"))]$price.1k, 5)

product[, list(name, price)]
products[, .(name, price)]
products[, c("name", "price"), with = F]

products[order(price), .(name = head(name),
                         price = head(price))]
products[, .(price = sum(price))]

a = products[, list(name.with.brand = paste0(brand, "-", name))]
a[order(name.with.brand)]

products[, list(name.with.brand = paste0(brand, "-", name))][order(name.with.brand)]

products[, .(price = {
  a = mean(price)
  b = median(price)
  c(min(price), max(price), a/b)
})]

# Step 8
products[, .(mean.price = mean(price)), by = brand]

products[order(-price), .(name = head(name, 3),
                          price = head(price, 3)),
         by = brand]

# Problem 1 
sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))

filter.expensive.available <- function(products, brands) {
  products[(price >= 500000) & (available == T) & (brand %in% brands)]
}

filter.expensive.available(sample.products, x)

# Alternative solution
filter.expensive.available <- function(products, brands) {    
  products[brand %in% brands][price >= 500000][available == T]}

# Problem 2 
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(purchases) {
  purchases[order(-price)][quantity > 0][,.(ordernumber,product_id)]
}

ordered.short.purchase.data(sample.purchases)

# Problem 3 
sample.purchases_2 <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

purchases.median.order.price <- function(purchases) {
  as.numeric(purchases[quantity > 0][,.(price = price*quantity), by = ordernumber][,.(price = sum(price)), by = ordernumber][,.(median(price))])
}

purchases.median.order.price(sample.purchases_2)

               