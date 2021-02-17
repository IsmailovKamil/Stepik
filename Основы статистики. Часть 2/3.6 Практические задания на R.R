# Problem 1 
dist_matrix <- dist(swiss) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data)

smart_hclust <- function(test_data, cluster_number){
  m = dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit2 = hclust(m, method = "complete", members = NULL)
  test_data$cluster <- as.factor(cutree(fit2, cluster_number))
  return(test_data)
}

smart_hclust(test_data, 2)

# Problem 2 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference <-  function(test_data, cluster_number){
    m = dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
    fit2 = hclust(m, method = "complete", members = NULL)
    cluster <- as.factor(cutree(fit2, cluster_number))
    x = sapply(test_data, function(x) summary(aov(x~cluster,test_data))[[1]]$'Pr(>F)'[1])
    return(names(which(x < 0.05)))
}

get_difference(test_data,2)

# Problem 3 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc <- function(d){
  p = prcomp(d)
  d$PC1 = p[["x"]][,1]
  d$PC2 = p[["x"]][,2]
  return(d)
}

get_pc(test_data)

# Problem 4 
get_pca2 <- function(data){
  fit <- prcomp(data)
  s = summary(fit)[["importance"]][3,]
  pc = as.data.frame(fit[["x"]])
  data = cbind(data,pc[,c(which(s < 0.9), which(s > 0.9)[1])])
  return(data)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pca2(swiss)

# Problem 5 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- as.data.frame(list(V1 = c(9, 18, 8, 4, 9), V2 = c(-7, -5, -5, -7, -11), V3 = c(6, 16, -6, 14, 14), V4 = c(2, 12, -10, 10, 10), V5 = c(12, 10, 10, 12, 16)))

is_multicol <- function(test_data){
  library("psych")
  corr = corr.test(test_data)
  o = as.data.frame(as.table(corr$r))
  z = c()
  for (i in 1:nrow(o)) {
    if (o[i,1] != o[i,2] & abs(o[i,3]) == 1)  {
      z = c(z,o[i,1])} 
  }
  if (length(z) != 0) {
    return(names(test_data)[z])
  } else return("There is no collinearity in the data")
}
  
is_multicol(test_data)

# Alternative solution 
is_multicol <- function(d){
  x = abs(cor(d))
  diag(x) = 0
  xx = rownames(which(x>.999, arr.ind = T))
  ifelse(is.null(xx), return("There is no collinearity in the data"), return(xx))
}

# Problem 6
library(ggplot2)
m = dist(swiss, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
fit2 = hclust(m, method = "complete", members = NULL)
swiss$cluster <- as.factor(cutree(fit2, 2))

ggplot(swiss, aes(Education, Catholic, col = cluster)) +
  geom_point() +
  geom_smooth(method = "lm")
