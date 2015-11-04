## Stat 420 HW 8 Q2
## install.packages("faraway")
library(faraway)
data(teengamb)
?teengamb
View(teengamb)

##Plot of gamble expenditure vs. socioeconomic status (males in black)
plot(teengamb$status, teengamb$gamble, col = teengamb$sex+1)

## Plot of gamble expenditure vs. income in pounds per week (males in red) 
plot(teengamb$income, teengamb$gamble, col = teengamb$sex+2)

## Plot of gamble expenditure vs. verbal score (males in green)
plot(teengamb$verbal, teengamb$gamble, col = teengamb$sex+3)

## The plots suggest the need for interaction terms because the data for males and females will have different regression lines. 
## Female regression line tends to be less steep 

test_fit <- lm(gamble~., data = teengamb)
summary(test_fit)
## The p value for the sex indicator is significant which also indicates that multiple lines might be appropriate.

## Fit a model with gamble as a response with sex interaction terms. 
fit1 <- lm(gamble ~ status + income+ verbal + sex + status:sex + income:sex + verbal:sex, data = teengamb)
summary(fit1)
## Gamble accounts for 62% of the variation explained by this model. 

fit2 <- lm(gamble ~ income + sex + income:sex, data = teengamb)
summary(fit2)
anova(fit1, fit2)
## Do not reject the Null Hypothesis. **We prefer smaller model with income and sex. 

