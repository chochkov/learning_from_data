library(data.table)

set.seed(12)

x2.given.x1.and.line <- function(x, l) {
  (l$y1 - l$y2) / (l$x1 - l$x2) * (x - l$x1) + l$y1;
}

dataset <- function(N) {
  l <- new.line()

  d = data.table(x1 = runif(N, -1, 1), x2 = runif(N, -1, 1))
  d[x2 <= x2.given.x1.and.line(x1, l), y:=-1]
  d[x2 >  x2.given.x1.and.line(x1, l), y:=+1]
  d[, y_hat:=0]
  d
}

new.line <- function() {
  list (
    x1 = runif(1, min=-1, max=1),
    y1 = runif(1, min=-1, max=1),
    x2 = runif(1, min=-1, max=1),
    y2 = runif(1, min=-1, max=1)
  )
}

# # plot
# plot(c(x1, x2), c(y1, y2), type='l', xlim=c(-1, 1), ylim=c(-1, 1))
# d[y==1, points(x1, x2, col='red')]
# d[y==-1, points(x1, x2, col='blue')]

# R repetitions, N 10 datapoints, w weights
R <- 1e3
N <- 10
w <- numeric(3)
iterations <- numeric(R)

for (r in 1:R) {
  d = dataset(N)

  i <- 0
  while (TRUE) {
    i <- i + 1

    d[, y_hat:=sign(w %*% rbind(1, x1, x2)), by=c('x1', 'x2')]

    m <- d[y != y_hat]

    if (nrow(m) == 0) break;

    w = m[sample.int(nrow(m), 1), w + y * c(1, x1, x2)]
  }

  iterations[r] <- i
  r <- r + 1

  if (r %% 5 == 0) cat('.')
}

cat(sprintf('\nAverage from %d repetitions: %f \n', R, mean(iterations)))
