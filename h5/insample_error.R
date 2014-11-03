sigma <- .1
d <- 8

error <- function(n) { sigma**2 * (1 - (d+1) / n) }

print(error(10))
print(error(25))
print(error(100))
print(error(500))
print(error(1000))
