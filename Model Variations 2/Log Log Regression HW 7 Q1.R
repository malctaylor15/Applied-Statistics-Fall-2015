library(MASS)
data(mammals)
View(mammals)
?mammals

## Find min and max of the brain and body data sets 
min(mammals$body)

max(mammals$body)

min(mammals$brain)

max(mammals$brain)

## Create model and plot the brain weight vs body weight 
fit <- lm(brain ~ body, data = mammals)
summary(fit)
plot(mammals$body,mammals$brain)
abline(fit)
## There are two data points with much higher orders of magnitude. 
## All of the observations are positive and several of the observations greater than one magnitude apart, whihc suggests that we can use logarithm of the variables. 

## Create model of the brain vs. the log of the body weight
log_fit1 <- lm(brain~ log(body), data = mammals)
## Use box cox to confirm that 0 is a recommended value for lambda 
boxcox(log_fit1, plotit = TRUE)
## Lambda equals 0 is above the 95% range which means it is a "recommended" value of lambda. 
## The log of the brain weight is a good response to the log of the body weight. 

log_fit <- lm(log(brain)~ log(body), data = mammals)
## Create linear model of the brain to the log of the body observations 
summary(log_fit)
plot(log(mammals$body),log(mammals$brain))
abline(log_fit)
## Log- log plot of the average brain weight and body weight 

qqnorm(resid(log_fit))
qqline(resid(log_fit))
## The residuals are very close to the line. They are normally distributed. 

hist(resid(log_fit))
## The histogram also looks normally distributed. 

hippo <- data.frame(body = 2300)

predict(log_fit,hippo, interval = c("prediction"),level = 0.95)
