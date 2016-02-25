### segment 1 
require(ISLR)
names(Smarket)
summary(Smarket)

pairs(Smarket, col=Smarket$Direction)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit, type="response")
glm.probs[1:5]

# classify them using a probability threshold of 0.5
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")

attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)

train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# now try and fit a smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

summary(glm.fit)

#### segment 2
require(ISLR)
require(MASS)

lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit

# hmm looks like normal distributions in each group
# looks like there's not much difference-- not a surprise because if we could
# predict we would be making lots of money!
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest Neighbors
## does the best about 1/3 of the time
library(class)
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
# 0.5! no better than flipping a coin!

## play from R section in Ch 4
pairs(Smarket) # correlations across all variables
cor(Smarket) # Direction is qualitative

# 
cor(Smarket [,-9])
attach(Smarket)
# volume goes up a lot over time
plot(Volume)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial)
glm.probs=predict(glm.fit, type="response")

glm.pred=rep("Down", length(glm.probs))
glm.pred[glm.probs>.5]="Up"

# how did we do?
table(glm.pred,Direction)
mean(glm.pred==Direction)

