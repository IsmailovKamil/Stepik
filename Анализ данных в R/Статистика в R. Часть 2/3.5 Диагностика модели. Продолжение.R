# step 1
library("ggplot2")

ggplot(swiss, aes(Examination,Education)) +
  geom_point() +
  geom_smooth()

fit = lm(Education ~ Examination, swiss)
summary(fit)

swiss$Examination_squared = (swiss$Examination)^2

fit2 = lm(Education ~ Examination + Examination_squared, swiss)
summary(fit2)

anova(fit,fit2)

swiss$fit1 = fit$fitted.values
swiss$fit2 = fit2$fitted.values
swiss$res1 = fit$residuals
swiss$res2 = fit2$residuals
swiss$obs_mum = 1:nrow(swiss)

ggplot(swiss, aes(Examination,Education)) +
  geom_point() +
  geom_line(aes(Examination, fit1), col = "blue") +
  geom_line(aes(Examination, fit2), col = "red")

library("gvlma")
x = gvlma(fit)
summary(x)

# Problem 1
data = read.csv("homosc.csv")
fit3 = lm(DV ~ IV, data)
y = gvlma(fit3)
summary(y)

resid.norm = function(fit){
  library("ggplot2")
  if (shapiro.test(fit$residuals)$p.value < 0.05) {
    ggplot(fit, aes(fit$residuals)) +
      geom_histogram(fill = "red")
  } else ggplot(fit, aes(fit$residuals)) +
      geom_histogram(fill = "green")
}

# Problem 2
high.corr <- function(x){
  c = as.data.frame(as.table(cor(x)))
  d = data.frame()
  for (i in 1:nrow(c)) {
    if (c[i,1] != c[i,2]) {
    d = rbind(d,c[i,])
    }
  }
  if (max(d[,3]) > abs(min(d[,3]))) {
    y = max(d[,3])
  } else y = min(d[,3])
  d = subset(d, d[,3] == y)
  return(as.character(d$Var2, d$Var1))
}

