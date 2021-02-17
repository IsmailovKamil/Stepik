# dataset 
library(mlmRev)
library(lme4)

# Main structure 
# lmer(dependent variable ~ fixed effect + (1 + fixed effect|random effect), data)
data('Exam')

str(Exam)
help(Exam)

library(ggplot2)
ggplot(Exam, aes(standLRT, normexam)) +
  geom_point()

ggplot(Exam, aes(standLRT, normexam, col = school)) +
  geom_point()

# One main effect 

Model_1 = lm(normexam ~ standLRT, Exam)
summary(Model_1)

Exam$Model1_pred = predict(Model_1)

ggplot(Exam) +
  geom_point(aes(standLRT, normexam)) + 
  geom_line(aes(standLRT, Model1_pred), col = "red", size = 1)

# Main effect + random effect 
Model_2 = lmer(normexam ~ standLRT + (1|school), Exam)
summary(Model_2)

Exam$Model2_pred = predict(Model_2)
ggplot(Exam) + 
  geom_point(aes(standLRT, normexam), alpha = 0.2) + 
  geom_line(aes(standLRT, Model2_pred, col = school))

# Main effect + random effect + random coefficient
Model_3 = lmer(normexam ~ standLRT + (1 + standLRT|school), data = Exam)
summary(Model_3)

Exam$Model3_pred = predict(Model_3)
ggplot(Exam) + 
  geom_point(aes(standLRT, normexam), alpha = 0.2) +
  geom_line(aes(standLRT, Model3_pred, col = school))

# Main effect + random coefficient
Model_4 = lmer(normexam ~ standLRT + (0 + standLRT|school), data = Exam)
summary(Model_4)

Exam$Model4_pred = predict(Model_4)
ggplot(Exam) + 
  geom_point(aes(standLRT, normexam), alpha = 0.2) +
  geom_line(aes(standLRT, Model4_pred, col = school))

# Uncorrelated random effect 
Model_5 = lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), Exam)
summary(Model_5)

