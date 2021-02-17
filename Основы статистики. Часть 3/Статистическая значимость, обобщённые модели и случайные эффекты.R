library(mlmRev)
library(lme4)
data('Exam')

# Model Comparison 
Model_2 = lmer(normexam ~ standLRT + (1|school), REML = F, Exam)
summary(Model_2)

Model_0 = lmer(normexam ~ 1 + (1|school), REML = F, Exam)
summary(Model_0)

anova(Model_0, Model_2)

# p-value
install.packages('lmerTest')
library('lmerTest')

Model_1 = lmer(normexam ~ standLRT + (1|school), REML = F, Exam)
summary(Model_1)

# Обобщенные смешанные модели 
Exam$school_type = ifelse(Exam$type == 'Mxd', 1, 0)

Model_5 = glmer(school_type ~ normexam + (1|school), family =
                  "binomial", data = Exam)
summary(Model_5)

# Prediction on new data 
predict(Model_2, Exam)

new_Exam = Exam[sample(1:nrow(Exam), 100),]
new_Exam$school = sample(101:200)

predict(Model_2, new_Exam, allow.new.levels = T)

# Исследование случайных эффектов 

fixef(Model_2)
ranef(Model_2)
