## install.packages("faraway")
library(faraway)
data(longley)
View(longley)
?longley

## Find the correlation between variables in the dataset
pairs(longley)
round(cor(longley),4)
## Several of the variables are highly correlated with other variables because many of the values in the chart are close to 1. 

## Create a model with Employed as a response 
fit <- lm(Employed ~., data = longley)
summary (fit)
## Calculate the variance inflation factor 
vif(fit)
## All of the inflation factors except for the Armed Forces is greater than 5 which suggests a multicolinearity issue.

pop_fit <- lm(Population ~ . -Employed, data = longley)
summary(pop_fit)
## Population is explained 99.75% by a linear relationship with the other variables. 

no_pop_fit <- lm(Employed ~ .-Population, data = longley)

cor(no_pop_fit$residuals, pop_fit$residuals)
## There is a weak correlation between population and employment because the value is much closer to 0 than 1. 

## Create new model of employed as a response to significant predictors 
employed_sig <- lm(Employed ~ Unemployed + Armed.Forces+Year, data = longley)

## Calculate Variance inflation factors 
vif(employed_sig)
## The VIFs do not suggest multi colinearity, they are all below 5. 

anova(employed_sig, fit)
## Reject Null that they are equal. Prefer employed_sig model with fewer predictor variables. 
