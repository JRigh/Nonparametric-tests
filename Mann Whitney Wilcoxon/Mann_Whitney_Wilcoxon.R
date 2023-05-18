#-------------------------------------------
# Mann-Whitney-Wilcoxon (rank sum) test in R
#-------------------------------------------

# last updated 11.03.2023

# 1. Mann-Whitney-Wilcoxon using inbuilt function in R (example slide 6)
# (at 0.05 level of significance)

set.seed(2023)
X = runif(10, min = 0, max = 2)
Y = runif(10, min = 0.2, max = 2.2)

wilcox.test(X,Y, alternative="two.sided", conf.level = 1-alpha) 
# Wilcoxon rank sum exact test
# 
# data:  X and Y
# W = 27, p-value = 0.08921
# alternative hypothesis: true location shift is not equal to 0

# boxplot

library(tidyverse)
data <- data.frame(value = c(X, Y), group = c(rep('X', length(X)), rep('Y', length(X))))
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
       subtitle = "Color gradient indicate the mean of the variable 'group' = X or Y",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

# Conclusion: using a point estimate of the p-value, we can NOT reject
# H0: Lx = Ly since p-value > 0.05

# 2. Asymptotic Mann-Whitney-Wilcoxon using inbuilt function in R (example slide 7)

set.seed(2023)
X = runif(10, min = 0, max = 2)
Y = runif(10, min = 0.2, max = 2.2)
S = c(X, Y)
R = rank(S)
Rx = sum(R[1:length(X)])
W = 2*Rx - (12*(N+1))
varW = 50*50*((N+1)/3)

pnorm(W, mean = 0, sd = sqrt(varW), lower.tail = FALSE) 
# 0.0008313872

# Conclusion: using the asysmptotic approximation, we reject H0: Lx = Ly that the both
# data come from the same generating distribution.


# 3. Distribution of Rx and W under H0 using simulation
set.seed(2023)
n = 100000
X = matrix(rep(0, 12*n), nrow = n, ncol = 12) 
Y = matrix(rep(0, 8*n), nrow = n, ncol = 8)
S = R = matrix(rep(0, (12+8)*n), nrow = n, ncol = 12+8)
Rx = W = numeric(n)

for(i in 1:n) {
  X[i,] = runif(12, min = 0, max = 2)
  Y[i,] = runif(8, min = 0, max = 2)
  S[i,] = c(X[i,], Y[i,])
  R[i,] = rank(S[i,])
  Rx[i] = sum(R[i, 1:12])
  W[i] = 2*Rx[i] + (12*(N+1))
}

# mean and variance of Rx and W
mean(Rx); var(Rx)
# [1] 126.048 about 12*(20+1)/2
# [1] 168.2862 about 12*8*(20+1)/12
mean(W); var(W)
# [1] 504.096 about (2*(12*(20+1)/2)) + 12*(20+1)
# [1] 673.145 

# plots
par(mfrow = c(1,2))
plot(density(Rx), main = 'Distribution (KDE) of Rx')
plot(density(W - rep((2*(12*(20+1)/2)) + 12*(20+1), n)), main = 'Centered distribution (KDE) of W')

#----
# end
#----

☺
