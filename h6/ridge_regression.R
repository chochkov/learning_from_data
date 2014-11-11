rm(list=ls())

library(data.table)

print.errors <- function(data.in, data.out) {
  cat(sprintf('In-sample error: %e\n', nrow(data.in[y_hat != y]) / nrow(data.in)))
  cat(sprintf('Out-of-sample error: %e\n', nrow(data.out[y_hat != y]) / nrow(data.out)))
}

regularized.lm <- function(lambda) {
  Z <- data.in[, cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2))]
  y <- data.in[, y]

  w  <- solve(t(Z) %*% Z + lambda * diag(8)) %*% t(Z) %*% y

  data.in[, y_hat:=sign(cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2)) %*% w)]
  data.out[, y_hat:=sign(cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2)) %*% w)]

  cat(sprintf('\n*** Regularised with Lambda: %e\n', lambda))

  print.errors(data.in, data.out)
}

data.in  <- data.table(read.table('in.dta', col.names=c('x1', 'x2', 'y')))
data.out <- data.table(read.table('out.dta', col.names=c('x1', 'x2', 'y')))

fit <- lm(y ~ x1 + x2 + I(x1**2) + I(x2**2) + I(x1 * x2) + I(abs(x1 - x2)) +
          I(abs(x1 + x2)), data=data.in)

data.in[, y_hat:=sign(predict(fit, newdata=data.in))]
data.out[, y_hat:=sign(predict(fit, newdata=data.out))]

cat('*** No regularisation:\n')
print.errors(data.in, data.out)

# problem 3
regularized.lm(1e-3)

# problem 4
regularized.lm(1e3)

# problem 5
regularized.lm(1e2)
regularized.lm(1e1)
regularized.lm(1e0)
regularized.lm(1e-1)
regularized.lm(1e-2)
