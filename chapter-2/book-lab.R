# chapter 2
require(ISLR)

p = c(0.5, 0.5)
O = matrix(c(1,2,3,4) ,2,2,byrow=TRUE)
sqrt(x)

x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)

# use set.seed(n) to produce consistent test data
set.seed(3)
y=rnorm(100)
mean(y)

# simple plots
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis", main="Plot of X vs Y")


x=seq(-pi,pi,length =50)
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour (x,y,f,nlevels =45,add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

# 3 dimensional
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta =30)
persp(x,y,fa,theta =30,phi =20)
persp(x,y,fa,theta =30,phi =70)
persp(x,y,fa,theta =30,phi =40)

# clean up by removing the rows with missing data
Auto=na.omit(Auto)
dim(Auto)
names(Auto)

# make the plotting easier by attaching the names in the runtime
attach(Auto)

# treat cylinders as qualitative/categorical
cylinders=as.factor(cylinders)

plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab="cylinders ", ylab="MPG")

# odd there are very few 5 cylinders
filter(Auto, cylinders == 5)

# histograms
hist(mpg)
hist(mpg, col=2)

# scatter plot matrix
pairs(Auto)

# limit the variables
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)

cor(horsepower, displacement)
cor(weight, acceleration)

# plot the points
plot(horsepower ,mpg)
# allows you to click on the point and see the name!!!
identify(horsepower ,mpg ,name)

summary(Auto)
