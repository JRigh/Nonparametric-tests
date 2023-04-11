#--------------------
# Sign test in Python
#--------------------

import numpy as np
from scipy.stats import binom
import statsmodels 
from statsmodels.stats.descriptivestats import sign_test

# data
data = [275,292,281,284,285,283,290,294,300,284]

# We test: H0: median(data) = 280

# sign test statistic
dataminusmed0 = data - np.repeat(280, 10)
dataminusmed0

S = (np.sum(np.sign(dataminusmed0)) + len(data) ) / 2
S # 9.0

# p-value
2*binom.pmf(1, n = 10, p = 0.5)
# 0.019531250000000003

# p-value with method sign_test() from statsmodels
sign_test(data, mu0=180)
# (5.0, 0.001953125)

#----
# end
#----