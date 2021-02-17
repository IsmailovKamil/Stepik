library(data.table)

# Step 2
setwd("~/OneDrive - George Mason University/Stepik/
      Анализ данных в R. Часть 2/Продвинутая предобработка данных/stepik")

products = fread("products.csv")


products[price < 1000, 
         name.with.price := paste0(name, "(", price, " rub.)")]
products[order(-price)]

products[, price := price / max(price), by = brand]

# Step 3
purchases = fread("purchases.csv")

setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

key(purchases)
key(products)

merge(purchases, products, by = "product_id")
merge(purchases, products, by.x = "product_id", by.y = "product_id")
merge(purchases, products, all.x = T, all.y =  F)

purchases[products, on = "product_id"]
purchases[products]

# J, SJ, CJ
products[J(c(158, 208, 10001))]
products[data.table(
  c(158, 208, 10001)
)]
products[.(c(158, 208, 10001))]
products[list(c(158, 208, 10001))]

print(SJ(c(158, 208, 10001)))
key(SJ(c(158, 208, 10001)))

print(CJ(c(158, 208, 10001),
         c("Supra", "Func")))
key(CJ(c(158, 208, 10001),
         c("Supra", "Func")))

# Step 5 
purchases.with.brand = merge(
  purchases,
  products[, list(product_id, brand)],
  by = "product_id"
)

pop.20.brands = head(
  purchases.with.brand[,list(
    total.brand.users = length(unique(externalsessionid))
  ),
  by = brand][order(-total.brand.users)], 20
)
