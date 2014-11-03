rm(list=ls())

library(data.table)
source('../dataset.R')

set.seed(12)

# R repetitions, N datapoints, LR learning rate
R <- 1e2
N <- 1e2
LR <- 1e-2
TESTING <- 1e3

cross.entropy.error <- function(y, w, x) {
  log(1 + exp(-y * (t(w) %*% x)))
}

qi <- function(y, w, x) {
  -(y * x) / (1 + exp(y * (t(w) %*% x)))
}

l2.norm <- function(v) {
  sqrt(sum(v ** 2))
}

epochs <- numeric(R)
errors <- numeric(R)
for (r in 1:R) {
  d <- dataset(N + TESTING)
  d[, index:=1:.N]

  w <- c(1, 0, 0)
  w.diff <- c(1, 1, 1)

  epoch <- 0
  while(l2.norm(w.diff) > 1e-2 && epoch < 2e3) {
    index <- sample(1:N, N)

    w.prev <- w
    for(i in index) {
      x <- d[i, c(1, x1, x2)]
      y <- d[i, y]

      w <- w - LR * qi(y, w, x)
    }
    w.diff <- w - w.prev
    epoch <- epoch + 1
    cat(sprintf('Epoch %d. Weights %e, %e, %e. Diff %0.7e \n', epoch, w[1], w[2], w[3], l2.norm(w.diff)))
  }

  epochs[r] <- epoch
  errors[r] <- d[, cross.entropy.error(y, w, c(1, x1, x2)), by=index][(N+1):(N+TESTING), mean(V1)]

  cat(sprintf('Repetition %d. Epochs to converge: %d. Error: %e\n', r, epochs[r], errors[r]))
}

cat(sprintf('Average epochs to converge: %e\n', mean(epochs)))
cat(sprintf('Average entropy error: %e\n', mean(errors)))
