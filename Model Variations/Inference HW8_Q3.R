corporations <- read.csv('E:/Stat Due 4 3/corporations.csv')

## Least squares prediction equation is y = B0 + B1x1 + B2x2 + e

## Fit overall model with B3 = 0 (without interaction terms)
fit_overall <- lm(profit ~ income + stock, data = corporations)
summary(fit_overall)

## Fit model with interaction terms 
fit_interact <- lm(profit~ income + stock+ stock:income, data = corporations)
summary(fit_interact)


## Test Null Hypothesis that B 3= 0 
anova(fit_overall,fit_interact)
## Reject the Null that B3 = 0 and we want the model with the interaction terms. 

## 
summary(fit_interact)
## The p-value for the interaction term is below the significance level which indicates there is  evidence that the variables interact. 

## The beta value for the income estimator is 0.0512 million per thousand dollar increase in CEO income when the stock percentage is constant. 

## Check the change in profit for every one thousand dollar increase create new CEO who owns 2% of stock
new_ceo1 <- data.frame(income= 1, stock = 2)
a<-predict(fit_interact, new_ceo1)

new_ceo2 <- data.frame(income= 2, stock = 2)
b <-predict(fit_interact, new_ceo2)

b-a




