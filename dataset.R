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

