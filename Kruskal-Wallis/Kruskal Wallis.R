#--------------------
# Kruskal-Wallis test 
#--------------------

#----------------
# working example
#----------------

# number of samples
k=4

# data
s1 <- c(15.791,12.595,10.405,9.836,8.729,9.608)
s2 <- c(16.193,11.937,11.968,9.376,9.227,8.539) 
s3 <- c(11.870,9.400, 9.322, 8.200,8.020,8.671)
s4 <- c(9.681, 9.764, 9.154, 8.330,8.388,7.888)

# visualization: boxplots

# boxplot

library(tidyverse)
data <- data.frame(value = c(s1,s2,s3,s4), group = c(rep('s1', length(s1)), rep('s2', length(s2)),
                                              rep('s3', length(s3)), rep('s4', length(s4))))
data <- data %>% 
  group_by(group) %>%
  mutate(Mean = mean(value))

ggplot(data, aes(x = group, y = value)) +
  geom_boxplot(aes(fill = Mean)) +
  geom_point(aes(x = group, y = value), position = 'jitter', size = 0.8) +
  scale_fill_gradient2('Mean', low = "aliceblue",
                       mid = "white", high = "coral",
                       midpoint = mean(data$Mean)) +
  labs(title = 'Boxplot for value x group',
       subtitle = "Color gradient indicate the mean of the variable 'group' = s1, s2, s3 or s4",
       caption = "Seabelt dataset") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))

# total sample size N = sum nj
N <- length(s1)+length(s2)+length(s3)+length(s4)

# matrix containing the ranks Rj in the pooled sample
Rk <- matrix(rank(c(s1,s2,s3,s4)), ncol = k, byrow = FALSE) # vector of ranks in pooled sample
Rk

# mean of the Rj
Rbar <- apply(Rk,2,mean)
Rbar  # [1] 17.00 15.50  9.17  8.33 (R_bar_j)

# test statistic
KW <- 12/(N*(N+1)) * sum( 6 * ( Rbar - (N+1)/2   )^2 )
KW  # [1] 6.926667 


# The sample sizes are large, under H0, K has approximately a chi-squared(k-1) distribution, 
# where k is the number of samples.
qchisq(p = 0.95 , df = 3)  # [1] 7.814728
# the 95% quantile is 7.814728 and so KW = 6.926667 is not in 
# the critical region if we take alpha = 5%.
# Thus we don't reject H0.

# 2. using the implemented function in R

# data
s1 <- c(15.791,12.595,10.405,9.836,8.729,9.608)
s2 <- c(16.193,11.937,11.968,9.376,9.227,8.539) 
s3 <- c(11.870,9.400, 9.322, 8.200,8.020,8.671)
s4 <- c(9.681, 9.764, 9.154, 8.330,8.388,7.888)

# 0.95 quantile of the Chi-squared distribution
qchisq(p = 0.95 , df = 3)
# [1] 7.814728

# plot chi-squared density for 3 degrees of freedom
set.seed(1986)
par(mar=c(5.1,4.1,4.1,2.1))
draws <- rchisq(10000000, 3)
dens <- density(draws)
plot(dens, xlab=expression(paste(" ", chi^2, )),
     xlim=c(0,18),
     ylab="PDF",
     main="P-values of the Kruskal-Wallis test as compared with a Chi-Squared distribution with 3 df",
     cex.main=1,)
q95 <- quantile(draws, .95)
q100 <- quantile(draws, 1.00)
x1 <- min(which(dens$x >= q95))  
x2 <- max(which(dens$x <  q100))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col='darksalmon'))
abline(v = KW)
text(9, 0.12, 'KW = 6.927', cex = 0.8)
text(2.5, 0.05, 'H0', cex = 0.9)
text(9.5, 0.02, 'H1', cex = 0.9)

#--------------------------------------
# Working example with inbuilt function
#--------------------------------------

# Kruskal-Wallis test with inbuilt function
kruskal.test(list(s1,s2,s3,s4))

# Kruskal-Wallis rank sum test
# data:  list(s1, s2, s3, s4)
# Kruskal-Wallis chi-squared = 6.9267, df = 3, p-value = 0.07427


#----------------------------
# Working example Simulations
#----------------------------

# simulate from the exact distribution

# Complementary exercice. Simulate from the exact distribution 
# of KW under H0 (which is not exactly a chi-squared)

# we are to simulate 4 samples of the same size 6 with the same distribution (any continuous)
# i.e. normal, uniform... and compute 100000 realizations of the test statistic

kruskal.distribution <- function(nj, nsim) {
  
  N = 4*nj
  KW <- rep(0,nsim)
  
  for(i in 1:nsim){
    
    s1 = runif(nj);  s2 = runif(nj)
    s3 = runif(nj);  s4 = runif(nj)
    Rk <- matrix(rank(c(s1,s2,s3,s4)), ncol = 4, byrow = FALSE)
    Rbarj <- apply(Rk,2,mean)
    KW[i] <- 12/(N*(N+1)) * sum(nj *(Rbarj -(N+1)/2)^2 )
  }
  
  output <- NULL
  output$KW <- KW
  output$quantiles <- quantile(x = KW, probs = c(0.05, 0.5, 0.95, 0.99))
  return(output)
}

set.seed(2021)
results <- kruskal.distribution(nj = 6, nsim = 100000)
results$quantiles
#   5%        50%       95%      99% 
#   0.380000  2.466667  7.446667 10.140000

# we find a 95% quantile of (with simulation) 7.45 which is 
# really lower than the previous one (7.814728).
# However the conclusion does not change: we don't reject H0

#----
# end
#----
