# step 1
rm(swiss)

swiss = data.frame(swiss)

fit_full = lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 = lm(Fertility ~ Examination + Education + Catholic 
                  + Infant.Mortality, data = swiss)
summary(fit_reduced1)

anova(fit_full,fit_reduced1)

fit_reduced2 = lm(Fertility ~ Agriculture + Education + Catholic 
                  + Infant.Mortality, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# model selection

optimal_fit = step(fit_full, direction = 'backward')

# Problem 1 
model_full <- lm(rating ~ ., data = attitude)
summary(model_full)

model_null <- lm(rating ~ 1, data = attitude)
summary(model_null)

scope = list(lower = model_null, upper = model_full)

ideal_model = step(model_null,scope, direction = "forward")

# Problem 2
anova(model_full,ideal_model)

# Problem 3
model <- lm(sr ~ .*., data = LifeCycleSavings)
summary(model)
