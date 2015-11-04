

ppm <- c(0,0,0,50,50,50,75,75,75,100,100,100,150,150,150,200,200,200)
mV <- c(1.72,1.68,1.74,2.04,2.11,2.17,2.40,2.32,2.33,2.91,3.00,2.89,
        4.47,4.51,4.43,6.67,6.66,6.57)
ises <- data.frame(ppm,mV)

## Plot mV vs ppm
plot(ppm,mV)

## Function to plot residuals 
plot_fit_res <- function(model){
  plot(fitted(model),resid(model), xlab = "Fitted", ylab = "Residuals")
  abline(h = 0)
}

## Linear model of the mV vs ppm 
lin_fit <- lm(mV~ppm)
abline(lin_fit)
summary(lin_fit)
plot_fit_res(lin_fit)
## No a linear model does not seem like a good fit. Most of the points are below the line and a straight line does not fit the shape of the points

## Fit quadratic model
quad_fit <- lm(mV ~ ppm + I(ppm^2))
summary (quad_fit)
plot_fit_res(quad_fit)
## The range of resiudals is much smaller for the quadratic fit. The linear fit ranged from -0.6 to 0.6 while the quadratic residuals ranged from -0.05 to 0.15. 

anova(lin_fit,quad_fit)
## Reject the Null and prefer the larger quadratic model.

## Plot quadratic curve
xplot <- seq(0,200,by = 1)
lines(xplot, predict(quad_fit, newdata = data.frame(ppm = xplot)), col = "blue")

