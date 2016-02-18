library(MASS)
library(ISLR)

names(Boston)
?Boston

# Simple linear regression
# response in medv (median housing prices) or medv is modeled as lstat
plot(medv~lstat, Boston)
fit1=lm(medv~lstat, data=Boston)

## REVIEW: F-statistic
summary(fit1)


# need to be done together
plot(medv~lstat, Boston)
abline(fit1, col="red")

names(fit1)
confint(fit1)

predict(fit1, data.frame(lstat=c(5, 10, 15)), interval="confidence")

# Multiple regression
fit2=lm(medv~lstat+age, data=Boston)
summary(fit2)

fit3=lm(medv~., Boston)
summary(fit3)

# from the first plot we know that there is some non linearity in the model; 
# we can explore that further here
# notice how there is some non-linearity in the residuals vs fitted
par(mfrow=c(2,2))
plot(fit3)


# same response as fit3 but minus age and minus indus
fit4=update(fit3, ~.-age-indus)
# everything in the model is not significant
summary(fit4)

### Non linear terms and interacions; star is interaction
fit5=lm(medv~lstat*age, Boston)
summary(fit5)

# by adding the I() it means interpret the inside literally. 
fit6=lm(medv~lstat + I(lstat^2), Boston)
summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat, fitted(fit6), col="red", pch=20) # adds the points from the quadratic fit

# the degree 4 polynomial over fits especially towards the right 
fit7=lm(medv~poly(lstat, 4))
points(lstat, fitted(fit7), col="blue", pch=20) # adds the points from the quadratic fit


# look at the plotting characters
plot(1:20, 1:20, pch=1:20, cex=2)

### qualitative predictors
# show data frame
fix(Carseats)
summary(Carseats)

# add interactions between income and advertising and age and price
fit1=lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fit1)

# shows how R will encode this factor or quantitative variable in regression
contrasts(Carseats$ShelveLoc)

# ... is unnamed and passed on as-is to where they are used
regplot=function(x, y, ...) {
  fit=lm(y~x)
  plot(x,y, ...)
  abline(fit, col="red")
}
attach(Carseats)
regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)


### quiz question
fit=lm(Sales~ Price*Age)
fit2=lm(Sales~ I(Price*Age))
