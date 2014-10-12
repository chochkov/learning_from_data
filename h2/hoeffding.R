library(data.table)

set.seed(12)

RR <- 1e5
CC <- 1e3
TT <- 1e1

# 1000 by 3 matrix to contain each repetition's result
repo <- matrix(nrow=RR, ncol=3)

to.toss <- function(rand) { if (rand > .5) 1 else 0 }

for (r in 1:RR) {
  c1 <- 1               # the first coin
  c2 <- runif(1, 1, CC) # a random coin
  c3 <- NA              # the first coin with minimum heads

  # 10 by 1000 matrix to store the tossing data for this repetition
  dat <- matrix(nrow=TT, ncol=CC)

  for (t in 1:TT) {
    dat[t, ] <- sapply(runif(CC), to.toss)
  }

  heads.ratios <- colSums(dat) / TT
  min.heads.ratio <- min(heads.ratios)

  i = 0;
  while (is.na(c3)) {
    i = i + 1
    if (heads.ratios[i] == min.heads.ratio) c3 = i
  }

  repo[r, 1] = sum(dat[, c1]) / TT # ratio of heads for coin 1
  repo[r, 2] = sum(dat[, c2]) / TT # ratio of heads for coin 1
  repo[r, 3] = sum(dat[, c3]) / TT # ratio of heads for coin 1
}

cat('Problem 6 \n')
print(colMeans(repo)/RR)
