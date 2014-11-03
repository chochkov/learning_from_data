# implement the ghost problem from the comments on problem 4

rm(list=ls())

LEARNING_RATE <- .05
MAX_SURFACE   <- 1e-14

E <- function(u, v) {
  (sin(u) * exp(-v) - (1 + u) * v ** 3) ** 2
}

du <- function(u, v) {
  2 * (sin(u) * exp(-v) - (1 + u) * v ** 3) * (cos(u) * exp(-v) - v ** 3)
}

dv <- function(u, v) {
  2 * (sin(u) * exp(-v) - (1 + u) * v ** 3) * (- sin(u) * exp(-v) - 3 * (1 + u) * v**2)
}

descend.u <- function(u, v) {
  u - LEARNING_RATE * du(u, v)
}

descend.v <- function(u, v) {
  v - LEARNING_RATE * dv(u, v)
}

u = .5
v = .5

i = 0
while (E(u, v) > MAX_SURFACE && i < 3) {
  i = i + 1

  cat(sprintf('U %e, V %e, E %e\n', u, v, E(u, v)))
  u.new = descend.u(u, v)
  v.new = descend.v(u, v)

  u = u.new
  v = v.new
}

print(i)
print(E(u, v))

test.u <- function(eps) {
  (E(1 + eps, 1) - E(1, 1)) / eps
}

test.v <- function(eps) {
  (E(1, 1 + eps) - E(1, 1)) / eps
}
