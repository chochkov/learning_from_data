library(data.table)

set.seed(12)

dataset <- function(N, with.noise=TRUE) {
  d = data.table(x1 = runif(N, -1, 1), x2 = runif(N, -1, 1))

  if (with.noise) d[sample(N, N * .1), `:=`(x1 = -x1, x2 = -x2)]

  d[, y:=target(x1, x2)]
}

# target function and 5 hypotheses
target <- function(x1, x2) sign(x1**2 + x2**2 - .6)

cat('*** Problem 8 ***\n')

R <- 1e3
N <- 1e3
repo <- matrix(nrow=R, ncol=2) # [, 1] untransformed, [, 2] transformed results

for (r in 1:R){
  d = dataset(N)

  untransformed.fit <- lm(y ~ x1 + x2, data=d)
  transformed.fit   <- lm(y ~ x1 + x2 + I(x1 * x2) + I(x1^2) + I(x2^2), data=d)

  d[, `:=`(
    y_hat_untransformed = sign(predict(untransformed.fit, d)),
    y_hat_transformed   = sign(predict(transformed.fit, d))
  )]

  repo[r, 1] <- nrow(d[y != y_hat_untransformed])
  repo[r, 2] <- nrow(d[y != y_hat_transformed])
  r = r + 1
}

errors <- colMeans(repo)
cat('Problem 8 result:\n')
print(errors[1] / N)

cat('Improved in-sample error by transformation:\n')
print(errors[2] / N)

cat('*** Problem 9 ***\n')

g1 <- function(x1, x2) sign(-1 - 0.05*x1 + 0.08*x2 + 0.13*x1*x2 +  1.5*x1**2 +  1.5*x2**2)
g2 <- function(x1, x2) sign(-1 - 0.05*x1 + 0.08*x2 + 0.13*x1*x2 +  1.5*x1**2 +   15*x2**2)
g3 <- function(x1, x2) sign(-1 - 0.05*x1 + 0.08*x2 + 0.13*x1*x2 +   15*x1**2 +  1.5*x2**2)
g4 <- function(x1, x2) sign(-1 -  1.5*x1 + 0.08*x2 + 0.13*x1*x2 + 0.05*x1**2 + 0.05*x2**2)
g5 <- function(x1, x2) sign(-1 - 0.05*x1 + 0.08*x2 +  1.5*x1*x2 + 0.15*x1**2 + 0.15*x2**2)

TT <- 100
testing <- dataset(TT, with.noise=FALSE)
testing[, `:=`(
  lr = sign(predict(transformed.fit, testing)),
  g1 = g1(x1, x2),
  g2 = g2(x1, x2),
  g3 = g3(x1, x2),
  g4 = g4(x1, x2),
  g5 = g5(x1, x2)
)]

cat('Problem 9\n')
cat(sprintf('g1 probability of agreeing with the transformed LR fit: %f\n', nrow(testing[lr==g1])/TT))
cat(sprintf('g2: %f\n', nrow(testing[lr==g2])/TT))
cat(sprintf('g3: %f\n', nrow(testing[lr==g3])/TT))
cat(sprintf('g4: %f\n', nrow(testing[lr==g4])/TT))
cat(sprintf('g5: %f\n', nrow(testing[lr==g5])/TT))

cat('*** Problem 10 ***\n')

RR <- SS <- 1e3

results <- sapply(1:RR, function(r) {
  d <- dataset(SS)
  fit <- lm(y ~ x1 + x2 + I(x1 * x2) + I(x1^2) + I(x2^2), data=d)

  d[, y_hat:=sign(predict(fit, d))]

  nrow(d[y != y_hat]) / SS
})

print(mean(results))
