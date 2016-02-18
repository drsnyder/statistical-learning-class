# 4.2.R1

dprob=function(b0, b1, balance) {
  num=exp(b0+ (b1 * balance))
  return (num / (1 + num))
}

b0=-10.6513
b1=0.0055
x=seq(1000, 2000, 1)
y=dprob(b0, b1, x)
df=data.frame(x=x, y=y)
head(df[df$y*100>=50,])
df[df$x==2000,]

# 4.3.R1
dprob1=function(b0, b1, x1, b2, x2) {
  num=exp(b0+ (b1 * x1) + (b2 * x2))
  return (num / (1 + num))
}
b0=-6
b1=0.05 # hours studied
b2=1 # undergrad gpa

dprob1(b0, b1, 40, b2, 3.5)

# 4.3.R2
hours=seq(40, 60, 1)
probs=dprob1(b0, b1, hours, b2, 3.5)
df=data.frame(hours, probs)
df[df$probs>=0.5,]
