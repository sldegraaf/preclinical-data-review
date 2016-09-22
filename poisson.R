# Preclinical biological data replication
# James Hicks

require(poibin)
require(tidyr)

# First, estimate the MidProb bound for a range of lambda using simulation

bound <- rep(NA, 2000)
names(bound) <- seq(1:2000)
for (lambda in 1:2000) {
  probs <- matrix(rpois(3 * 10000, lambda), ncol = 3, byrow = TRUE)
  logic <- apply(probs, 1, function (x) round(mean(x)) %in% x & range(x)>1)
  bound[lambda] <- sum(logic) / length(logic)
}

# Input colony counts for rts and other

colony <- read.csv("Bishayee Colony Counts 10.27.97-3.8.01.csv")
colony <- colony %>% drop_na(col1, col2, col3)
colony.other <- read.csv("Other Investigators in Lab.Colony Counts.4.23.92-11.27.02.csv")
colony.other <- colony.other %>% drop_na(col1, col2, col3)

# Test I

# replicate their numbers
1 - pbinom(689, 1343, .42)

# using our numbers
rts.means <- round(colony$average)
rts.probs <- bound[rts.means]
rts.k <- sum(apply(colony[, c("col1", "col2", "col3")],
                    1, function (x) round(mean(x)) %in% x))

rts.k
length(rts.means)
max(rts.probs)

1 - pbinom(rts.k - 1, length(rts.means), max(rts.probs))

# Test II
# Estimate probabilities using lambda MLE
# 1: Estimate poisson parameter for each triple (its mean)
# 2: Extract associated probability for each triple from bounds object
# 3: Feed that vector into -ppoibin- to calculate probability for P-B distribution being more than value in RTS and other, respectively

# RTS investigator
1 - ppoibin(rts.k - 1, rts.probs)

# Other investigators
other.means <- round(colony.other$average)
other.probs <- bound[other.means]
other.k <- sum(apply(colony.other[, c("col1", "col2", "col3")],
               1, function (x) round(mean(x)) %in% x))
1 - ppoibin(other.k - 1, other.probs)

# Test III
# Normal approximation of poisson binomial

# mu is sum(p); var is sum(p * q)
pn.mean <- sum(rts.probs)
pn.sd <- sqrt(sum(rts.probs * (1 - rts.probs)))

# z-score for rts.k (709)
(rts.k - pn.mean) / pn.sd
