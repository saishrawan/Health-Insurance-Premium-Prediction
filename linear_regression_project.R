setwd("C:/Users/sidda/OneDrive/Documents/TAMU/Academics/Spring/STAT 654/Project")
library(caret)
library(leaps)
#reading the data and preprocessing
datad = read.csv("insurance.csv")
datad$chargeslog = log1p(datad$charges)
datad[c(1, 3)] <- scale(datad[c(1, 3)])
train_control <- trainControl(method = "cv", number = 5)

#Leaps package to get the best predictors subset
step_model <- train(chargeslog ~ age + sex + bmi + children + smoker + region, data = datad,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train_control)
step_model$results
summary(step_model$finalModel)

#Fitting linear models for the best predictors subset and storing AIC, BIC
hc1 <- lm(datad$chargeslog ~datad$smoker)
BIC(hc1)
AIC(hc1)
summary(hc1)$r.squared

hc2 <- lm(datad$chargeslog ~datad$smoker + datad$age )
BIC(hc2)
AIC(hc2)
summary(hc2)$r.squared


hc3 <- lm(datad$chargeslog ~datad$smoker + datad$age + datad$children )
BIC(hc3)
AIC(hc3)
summary(hc3)$r.squared


hc4 <- lm(datad$chargeslog ~datad$smoker + datad$age + datad$bmi + datad$children )
BIC(hc4)
AIC(hc4)
summary(hc4)$r.squared


hc5 <- lm(datad$chargeslog ~datad$smoker + datad$age + datad$bmi + datad$children + datad$region )
BIC(hc5)
AIC(hc5)
summary(hc5)$r.squared

hc6 <- lm(datad$chargeslog ~ datad$smoker + datad$age + datad$bmi + datad$children + datad$sex +datad$region  )
BIC(hc6)
AIC(hc6)

#Linear model with all predictors
summary(hc6)$r.squared
summary(hc6)

resids = resid(hc5)
pred = hc5$fitted.values

#residual plots for the best predictor
plot(datad$age, hc5$residuals, xlab = "age", ylab = "residual")
plot(datad$bmi, hc5$residuals, xlab = "bmi", ylab = "residual")
plot(datad$children, hc5$residuals, xlab = "children", ylab = "residual")
plot(hc5$fitted.values, hc5$residuals, xlab = "fitted", ylab = "residual")

#summary for the best predictor
summary(hc5)

sqrt(mean(hc5$residuals^2))












