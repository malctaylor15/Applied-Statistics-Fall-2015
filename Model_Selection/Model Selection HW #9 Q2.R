## install.packages("faraway")
library(faraway)
library(broom)
data(teengamb)

fit2 <- lm(gamble ~., data = teengamb)

fit2_back_AIC <- step(fit2,direction = "backward")
## The best model according to backward AIC includes the verbal, sex and income predictors. 

n <- length(resid(fit2))
fit2_back_BIC <- step(fit2, direction = "backward", k = log(n))
## The best model according to backward BIC includes sex and income. 


glance(fit2)
## Adjusted R_squared is 0.48, the AIC is 433, BIC 444

glance(fit2_back_AIC)
## Adjusted R_squared is 0.49, AIC is 431, BIC 440

glance(fit2_back_BIC)
## Adjusted R_squared is 0.47, AIC 432 and BIC 439

## The backward AIC is the best model of the three because it has the highest Adjusted R_squared value. It also has the lowest AIc and second lowest BIC. 