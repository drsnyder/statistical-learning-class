p = c(0, 0, 0)
O = matrix(c(0, 3, 0, 2, 0, 0, 0, 1, 3, 0, 1, 2, -1, 0, 1, 1, 1, 1) ,6,3,byrow=TRUE)

# 7
# get the euclidean distance
apply(0, 1, sum)
# > [1] 3 2 4 3 0 3
# K=1 the only 0 is Green
# K=3 Red, Red, Green (or Red); red is 2/3 so it's Red

# 8 
fix(College)
# change the first column to the names
#rownames(College)=College[,1]
#fix(College)
summary(College)
# first 10 columns
pairs(College[,1:10])

# iii plot box plots of Outstate vs Private
attach(College)
Private=as.factor(Private)
plot(Private , Outstate, col ="red")

# iv
Elite=rep("No",nrow(College))
Elite[College$Top10perc >50]=" Yes"
Elite=as.factor(Elite)
College=data.frame(College , Elite)
plot(Elite, Outstate, col="red")

# v
par(mfrow=c(2,2))
hist(Apps)
hist(Top10perc)
hist(Outstate)
hist(Personal)

# 9
# (a)
# qualitative: origin (possibly year and cylinders)
# quantitative: the rest
# (b, c)
summary(Auto) # gives the min, max, mean
apply(Auto[,1:8], 2, range) # also gives the range of everything but the "name"
apply(Auto[,1:8], 2, sd) # also gives the range of everything but the "name"
# (d)
Autos = Auto[-c(10:85),1:8] # remove rows 10 to 85
summary(Autos) # gives the min, max, mean
apply(Auto[,1:8], 2, range) # also gives the range of everything but the "name"
apply(Auto[,1:8], 2, sd) # also gives the range of everything but the "name"
# (e)
pairs(Autos[,1:8])
cor(horsepower, acceleration)
# any of these would be good predictors of mpg
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)
cor(displacement, mpg)
cor(horsepower, mpg)

# there are some outliers. what are they?
plot(horsepower ,mpg)
identify(horsepower ,mpg ,name)
identify(distance ,mpg ,name)


