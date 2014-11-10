rm(list=ls())

library(data.table)

data.in  <- data.table(read.table('in.dta', col.names=c('x1', 'x2', 'y')))
data.out <- data.table(read.table('out.dta', col.names=c('x1', 'x2', 'y')))

fit <- lm(y ~ x1 + x2 + I(x1**2) + I(x2**2) + I(x1 * x2) + I(abs(x1 - x2)) +
          I(abs(x1 + x2)), data=data.in)

data.in[, y_hat:=sign(predict(fit, newdata=data.in))]
data.out[, y_hat:=sign(predict(fit, newdata=data.out))]

cat(sprintf('In-sample error: %e\n', nrow(data.in[y_hat != y]) / nrow(data.in)))
cat(sprintf('Out-of-sample error: %e\n', nrow(data.out[y_hat != y]) / nrow(data.out)))

regularized.lm <- function(lambda) {
  Z <- data.in[, cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2))]
  y <- data.in[, y]

  I8 <- matrix(1, nrow=8, ncol=8)
  w  <- solve(t(Z) %*% Z + lambda * I8) %*% t(Z) %*% y

  data.in[, y_hat_reg:=sign(cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2)) %*% w)]
  data.out[, y_hat_reg:=sign(cbind(1, x1, x2, x1^2, x2^2, x1 * x2, abs(x1 - x2), abs(x1 + x2)) %*% w)]

  cat(sprintf('Props for Lambda: %e\n', lambda))
  cat(sprintf('In-sample error: %e\n', nrow(data.in[y_hat_reg != y]) / nrow(data.in)))
  cat(sprintf('Out-of-sample error: %e\n', nrow(data.out[y_hat_reg != y]) / nrow(data.out)))
}

# problem 3
cat('Problem 3\n')
regularized.lm(1e-3)

# problem 4
cat('Problem 4\n')
regularized.lm(1e+3)

# problem 5
cat('Problem 5\n')
regularized.lm(1e+2)
regularized.lm(1e+1)
regularized.lm(10)
regularized.lm(1e-1)
regularized.lm(1e-2)
