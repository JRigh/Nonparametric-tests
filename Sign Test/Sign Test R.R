#---------------
# Sign test in R
#---------------

# data
data <- c(275,292,281,284,285,283,290,294,300,284)

# We test: H0: median(data) = 280

# sign test statistic
dataminusmed0 <- data -280
S = (sum(sign(dataminusmed0)) + length(data) ) / 2
S # 9

# p-value
2*pbinom(q=1, size = length(data), prob = 0.5)
# [1] 0.02148438

# with package BSDA
library(BSDA)

SIGN.test(
  x = data,
  y = NULL,
  md = 180,
  alternative = "two.sided",
  conf.level = 0.95)

#----
# end
#----