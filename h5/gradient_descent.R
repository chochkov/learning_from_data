# Problem 4 - implement gradiend descent on E(u, v)

rm(list=ls())

LEARNING_RATE <- .1
MAX_SURFACE   <- 1e-14

E <- function(p) {
  (p$u * exp(p$v) - 2 * p$v * exp(-p$u)) ** 2
}

du <- function(p) {
  2*((exp(p$v) + 2 * p$v * exp(-p$u)) * (p$u * exp(p$v) - 2 * p$v * exp(-p$u)))
}

dv <- function(p) {
  2*((p$u * exp(p$v) - 2 * exp(-p$u)) * (p$u * exp(p$v) - 2 * p$v * exp(-p$u)))
}

descend <- function(p) {
  new.u <- p$u - LEARNING_RATE * du(p)
  new.v <- p$v - LEARNING_RATE * dv(p)

  p$u <- new.u
  p$v <- new.v

  p
}

point <- list(u=1, v=1)

i = 0
while (E(point) > MAX_SURFACE) {
  i = i + 1

  point <- descend(point)
}

cat(sprintf('Surface after %d iterations: %e\n', i, E(point)))

# Test functions for the derivatives using the finite differences
test.du <- function(eps) { (E(1 + eps, 1) - E(1, 1)) / eps }

test.dv <- function(eps) { (E(1, 1 + eps) - E(1, 1)) / eps }
