##install.packages("faraway")
library(faraway)
data(prostate)
##install.packages("broom")
library(broom)

fit <- lm(lpsa~., data = prostate)
summary(fit)
## Remove gleason because p - value is highest and higher than alpha 

fitv1 <- lm(lpsa~.-gleason, data = prostate)
summary(fitv1)
## Remove lcp because p- value is highest and higher than alpha 
fitv2 <- lm(lpsa~.-gleason -lcp, data = prostate)
summary(fitv2)
## Remove pgg45 because p- value is highest and higher than alpha 
fitv3 <- lm(lpsa~.-gleason -lcp -pgg45, data = prostate)
summary(fitv3)
## Remove age because p- value is highest and higher than alpha 
fitv3 <- lm(lpsa~.-gleason -lcp -pgg45 -age, data = prostate)
summary(fitv3)
## Remove lbph because p- value is highest and higher than alpha 
fitv4 <- lm(lpsa~.-gleason -lcp -pgg45 - age -lbph, data = prostate)
summary(fitv4)
## Final model because all p-values below alpha level 
## Backward elimination model includes lcavol, lweight and svi 

fit_back_AIC <- step(fit,direction = "backward")
## Best model according to backward AIC includes lcavol, lweight, age and lbph and svi

glance(fit)
## Adjusted R_squared is 0.623
glance(fitv4)
## Adjusted R_squared is 0.614
glance(fit_back_AIC)
## Adjusted R_squared is 0.624

## The backward AIC is the best model because the Adjusted R_squared appears to be the best model. 

