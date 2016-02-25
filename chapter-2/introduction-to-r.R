
###

t = function(b1, b, se) {
  return ((b1 - b) / se);
}

se = 0.2
b1 = 0.5

# 3.2 R1
x = seq(0, 2, 0.01)
v = data.frame(x, t(b1, x, se))
v$y = abs(v$y)
max(v[v$y<2,]$x) # = 0.89
