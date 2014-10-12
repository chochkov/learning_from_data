library(data.table)

new.line <- function() {
  list(
    x1 = runif(1, min=-1, max=1),
    y1 = runif(1, min=-1, max=1),
    x2 = runif(1, min=-1, max=1),
    y2 = runif(1, min=-1, max=1)
  )
}

x2.given.x1.and.line <- function(x1, l) {
  (l$y1 - l$y2) / (l$x1 - l$x2) * (x1 - l$x1) + l$y1
}

R <- 1e3
N <- 1e2 # training dataset size
TT <- 1e3 # testing dataset size
repo <- matrix(nrow=R, ncol=2)

for (r in 1:R) {
  l <- new.line()

  # training dataset
  training <- data.table(x1 = runif(N, -1, 1), x2 = runif(N, -1, 1))
  training[x2 <= x2.given.x1.and.line(x1, l), y:=-1]
  training[x2 >  x2.given.x1.and.line(x1, l), y:=+1]

  # testing dataset
  testing <- data.table(x1 = runif(TT, -1, 1), x2 = runif(TT, -1, 1))
  testing[x2 <= x2.given.x1.and.line(x1, l), y:=-1]
  testing[x2 >  x2.given.x1.and.line(x1, l), y:=+1]

  # linear regression as classification
  fit <- lm(y ~ x1 + x2, data=training)

  # in-sample error
  training[, y_hat:=sign(predict(fit, training))]
  repo[r, 1] <- nrow(training[y != y_hat])

  # out-of-sample error
  testing[, y_hat:=sign(predict(fit, testing))]
  repo[r, 2] <- nrow(testing[y != y_hat])
}

cat('Problem 5 \n')
print(mean(repo[, 1])/N)

cat('Problem 6 \n')
print(mean(repo[, 2])/TT)
