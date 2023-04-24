#-----------------------------------
# Spearman's correlation coefficient 
# in Python
#-----------------------------------

import numpy as np

x = ([273, 285, 270, 272, 278, 276, 291, 290, 279, 292])
y = ([288, 284, 298, 271, 307, 301, 299, 295, 293, 352])

from scipy import stats
stats.spearmanr(x,y)

from scipy.stats import rankdata

def spearman(x,y):
    n = len(x)
    numerator = np.sum((rankdata(x, method='min') - (n+1)/2)*(rankdata(y, method='min') - (n+1)/2))
    denominator = (( np.sum(   np.square(rankdata(x, method='min') - (n+1) / 2) )) *np.sum( np.square(rankdata(y, method='min') - (n+1)/2))  )**0.5
    return(numerator/denominator)
    
spearman(x,y)

# examples of p-value calculation using large-sample approximations
np.random.seed(2023)
n=100 
x=np.random.uniform(0,1,n)
y=np.random.uniform(0,1,n)
stats.spearmanr(x,y, alternative='two-sided')

# SignificanceResult(statistic=0.020774077407740774, pvalue=0.8374540868726486)

from scipy.stats import norm

# p-value with standard Normal approximation
2*norm.sf(x=((n-1)**0.5 * spearman(x = x, y = y)), loc=0, scale=1)

# 0.8362445788532031

from scipy.stats import t

# p-value with student approximation
2*t.sf(x=((n-1)**0.5 * spearman(x = x, y = y)), df=n, loc=0, scale=1)

# 0.8366648646693274

#----
# end
#----