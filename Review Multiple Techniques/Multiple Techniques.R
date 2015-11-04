##install.packages("broom")
library(broom)
gpa <- read.csv("D:/BEFORE BU/Stat Exam Part 1/gpa.csv") 
View(gpa)

#Fit a linear model with two fraternities and time spent studying
fit1 <- lm(y ~ v1 +v2+x, data = gpa)
sum_f1<- summary(fit1)
sum_f1
## Fitted line is y = 1.6 -0.8v1 +0.2v2 +0.1x 

## Null hypothesis is that B1= B2 = 0 for the GPA being the same for all fraternities
aov(fit1)
## F values, p-values stated in table 
summary(aov(fit1))
## The p-values are below the 0.05 so we would reject the Null
##Test statistic 
qf(0.95,1,14)
## The F values are larger than the test statistic so we reject the Null 

## The Null Hypothesis is B3 = 0 for time having no effect on GPA 
## F values, p-values stated in table 
summary(aov(fit1))
## The p- value is below 0.05 so we would reject the Null hypothesis 
##Test statistic 
qf(0.95,1,14)

## Fit Model without intercept 
fit2 <- lm(y ~. +0, data = gpa)
sum_f2<- summary(fit2)
sum_f2
## Fitted Values of first model 
sum_f1

## Compare residuals of two models rounded to the first decimal place. 

## Fit 1 Residuals 
round(sum_f1$residuals,1)
##Fit 2 Residuals 
round(sum_f2$residuals,1)

## When rounded to the first decimal place, the residuals are the same 
round(sum_f2$residuals,1) == round(sum_f1$residuals,1)


## Fit Models for the models from parts (b) and (c) 

## Null model with B1 = B2 = 0
fit_n1 <- lm(y ~x, data = gpa)
## Null model with B3 = 0
fit_n2 <- lm(y~v1+v2, data = gpa)

glance(fit_n1)
## AIC is 31.88, BIC is 34.55, Adjusted R2 is 0.72
glance(fit_n2)
## AIC is 39.46, BIC is 43.02, Adjusted R2 is 0.59

## The AIC and BIC are lower for the first model which means we prefer the model without time. 
## The adjusted R2 is higher for the first model which means we prefer the model without time. 


## Create linear model with interaction terms for the two fraternities 
fit3 <- lm(y ~ v1*x+v2*x, data = gpa)
summary(fit3)
## Null Hypothesis B4 = B5 = 0
aov(fit3)
summary(aov(fit3))
## P values for interaction terms are above 0.05 so do not reject the null 
## Test statistic 
qf(0.95,1,12)
## The F value is below the critical value so we do not reject the null. 
## There is no interaction between time studying and fraternity. 