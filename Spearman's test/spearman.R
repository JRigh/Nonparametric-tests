#-----------------------------------
# Spearman's correlation coefficient 
# in R
#-----------------------------------

# Plots Spearman vs Pearson

par(mfrow = c(2,3))
set.seed(2021)
n = 30
x <- rnorm(n)
y1 <- x
y2 <- -x
y3 <- x^2
y4 <- x + rnorm(n, 0, 0.5)
y5 <- rnorm(n)
y6 <- exp(x)
plot(x, y1, main = paste("Spearman = ", round(cor(x,y1, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y1, method = 'pearson'),2)))
plot(x, y2, main = paste("Spearman = ", round(cor(x,y2, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y2, method = 'pearson'),2)))
plot(x, y3, main = paste("Spearman = ", round(cor(x,y3, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y3, method = 'pearson'),2)))
plot(x, y4, main = paste("Spearman = ",  round(cor(x,y4, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y4, method = 'pearson'),2)))
plot(x, y5, main = paste("Spearman = ", round(cor(x,y5, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y5, method = 'pearson'),2)))
plot(x, y6, main = paste("Spearman = ", round(cor(x,y6, method = 'spearman'),2),
                         "Pearson = ", round(cor(x,y6, method = 'pearson'),2)))

# data
x <- c(273, 285, 270, 272, 278, 276, 291, 290, 279, 292)
y <- c(288, 284, 298, 271, 307, 301, 299, 295, 293, 352)

# Spearman's correlation coefficient
cor.test(x, y, method = 'spearman', conf.level = 0.95,
         alternative = 'two.sided')

# Spearman's rank correlation rho
# 
# data:  x and y
# S = 100, p-value = 0.2629
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3939394 

plot(x,y)


# function that computes the Spearman's correlation coefficient
spearman <- function(x, y) {
  n <- length(x)
  numerator <- sum((rank(x) - (n+1)/2)*(rank(y) - (n+1)/2))
  denominator <- sqrt( sum((rank(x) - (n+1)/2)^2)*sum((rank(y) - (n+1)/2)^2))
  return(numerator/denominator)
}

spearman(x = x, y = y)
# [1] 0.3939394


# examples of p-value calculation using large-sample approximations
set.seed(2021)
n=100 ; x=runif(n) ; y=runif(n)
cor.test(x, y, method = 'spearman', conf.level = 0.95,
         alternative = 'two.sided')
# 
# Spearman's rank correlation rho
# 
# data:  x and y
# S = 160598, p-value = 0.7194
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.03631563 

# p-value with standard Normal approximation
2*pnorm(q=(sqrt(n-1) * spearman(x = x, y = y)), mean = 0, sd =1,
           lower.tail = FALSE)
# [1] 0.7178483

# p-value with Student's approximation
2*pt(q = (sqrt(n-2) * (spearman(x = x, y = y) / sqrt( 1 - (spearman(x = x, y = y))^2))),
        df = n-2,
        lower.tail = FALSE)
# [1] 0.7198129

#----
# end
#----