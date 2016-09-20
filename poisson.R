# Preclinical biological data replication
# James Hicks

require(poibin)
require(tidyr)

# First, estimate the MidProb bound for a range of lambda
# (1) analytic - currently unfinished!

is.odd <- function (x) x %% 2 != 0

midprob <- function (lambda, j, N) {
  prob <- NA
  for (j in 2:N) {
    prob[j] <- ppois(k - j, lambda) *
    (ppois(k - (j / 2), lambda) + is.odd(j) * ppois(k - (j / 2) - 1, lambda)) *
    ppois(k, lambda)
  }
  out <- 6 * sum(prob)
  return(out)
}

# (2) simulation

bound <- rep(NA, 1000)
names(bound) <- seq(1:1000)
for (lambda in 1:1000) {
  probs <- matrix(rpois(3 * 5000, lambda), ncol = 3, byrow = TRUE)
  logic <- apply(probs, 1, function (x) round(mean(x)) %in% x)
  bound[lambda] <- sum(logic) / length(logic)
}

# Test I:

colony <- read.csv("Bishayee Colony Counts 10.27.97-3.8.01.csv")
colony <- colony %>% drop_na(col1, col2, col3)
colony.other <- read.csv("Other Investigators in Lab.Colony Counts.4.23.92-11.27.02.csv")
colony.other <- colony.other %>% drop_na(col1, col2, col3)



# Test II

# RTS investigator
bish.means <- round(colony$average)
bish.probs <- bound[bish.means]
k <- sum(apply(colony[,c("col1","col2","col3")], 1, function (x) round(mean(x)) %in% x))
1-ppoibin(k-1, bish.probs)

# Other investigators
other.means <- round(colony.other$average)
other.probs <- bound[other.means]
k <- sum(apply(colony.other[,c("col1","col2","col3")], 1, function (x) round(mean(x)) %in% x))
1-ppoibin(k-1, other.probs)


# Test III:
