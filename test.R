library(data.table)

d <- data.table(read.table('data.txt', header=TRUE))

x1 <- 0.415126
y1 <- -0.796030
x2 <- -0.397647
y2 <- -0.373959

plot(c(x1, x2), c(y1, y2), type='l', xlim=c(-1, 1), ylim=c(-1, 1))

print(d[, .N, by=Y])

d[Y==1, points(X1, X2, col='red')]
d[Y==-1, points(X1, X2, col='blue')]
