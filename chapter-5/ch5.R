require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

## LOOCV
# can use glm for linear as well
glm.fit=glm(mpg~horsepower, data=Auto)

# delta is the prediction error
# 0 is cv, 1 is bias corrected
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)
summary(glm.fit)

# for linear regression only
##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)


cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


## Bootstrap
## Minimum risk investment - Section 5.2

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)

## What is the standard error of alpha?

alpha.fn=function(data, index){
  # can reference X and Y from data using with
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

## 5.R quiz
load("chapter-5/5.R.RData")
plot(y~X1+X2,data=Xy)
glm.fit=glm(y~X1+X2,data=Xy)
summary(glm.fit)

# get the SE for X1
summary(glm.fit)$coef[[5]]

# estimate SE of B1 (X1) by bootstrapping
boot.fn=function(data, index) {
   coef(lm(y~X1+X2,data=data, subset=index))
}
boot.fn(Xy,1:100)

set.seed(1)
boot.fn(Xy,sample(1000,1000, replace=T))

# using bootstrap
boot.out=boot(Xy, boot.fn, 1000)
boot.out
plot(boot.out)

# block bootstrap to estimate
new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
new.Xy = Xy[new.rows, ]
boot(new.Xy, boot.fn, 1000)

blocks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
explode_block=function(end) {
   seq(from=end-99, to=end)
}

# testing how this works with blocked bootstrap
# create a block
block.Xy = Xy[unlist(lapply(sample(blocks, 10, replace=T), explode_block)), ]
boot.fn(block.Xy,sample(1000,1000, replace=T))

# this is one way to do it correctly-- generate the index internally and ignore
# the input
block.fn2=function(data, ignore) {
  index=unlist(lapply(sample(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 10, replace=T), explode_block))
  coef(lm(y~X1+X2,data=data, subset=index))
}
block2.out=boot(Xy, block.fn2, 1000)
block2.out
