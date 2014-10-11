library(data.table)

set.seed(12)

x1 = runif(1, min=-1, max=1)
y1 = runif(1, min=-1, max=1)
x2 = runif(1, min=-1, max=1)
y2 = runif(1, min=-1, max=1)

threshold <- function(x) {
  (y1 - y2) / (x1 - x2) * (x - x1) + y1;
}

# dataset
N = 10
d = data.table(x1 = runif(N, -1, 1), x2 = runif(N, -1, 1))
d[x2 <= threshold(x1), y:=-1]
d[x2 > threshold(x1), y:=+1]
d[, y_hat:=0]

# plot
plot(c(x1, x2), c(y1, y2), type='l', xlim=c(-1, 1), ylim=c(-1, 1))
d[y==1, points(x1, x2, col='red')]
d[y==-1, points(x1, x2, col='blue')]
