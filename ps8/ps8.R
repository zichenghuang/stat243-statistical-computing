## problem 1

## (a)
library(VGAM)
x <- seq(0.1, 10, length = 10000)
pareto <- dpareto(x, 2, 3)
plot(x, pareto, type = "l", ylab = "p(x)")
lines(x, dexp(x), lty = 2)
legend("topright", lty = c(2, 1), legend = c("Exponential", "Pareto"), bty = "n")

## (b)
# setup
library(tolerance)
m <- 10000
alpha <- 2
beta <- 3

# get a sample of size 10000 from pareto distirbution
sample <- rpareto(m, alpha, beta)

# f follows exponential density with parameter equals to 1, shifted by two to the right
f <- d2exp(sample, rate=1, shift = 2)

# g follows Pareto distribution with alpha=2 and beta=3
g <- dpareto(sample, alpha, beta)

# estimate E(X)
mean(sample * f / g)
var(sample * f / g)

# calculate E(X^2)
mean(sample^2 * f / g)
var(sample^2 * f / g)

# Histograms of h(x)f(x)/g(x), h(x) = x
hist(sample * f / g, main = "Histogram of x(f(x)/g(x))")

# Histograms of h(x)f(x)/g(x), h(x) = x^2
hist((sample^2) * f / g, main = "Histogram of x^2(f(x)/g(x))")

# Histogram of weights f(x)/g(x)
hist(f / g, main = "Histogram of weights f(x)/g(x)")

#### c
# setup
m <- 10000 
alpha <- 2
beta <- 3

# get a sample of size 10000 from the shifted exponential distribution
sample1 <- r2exp(m, rate = 1, shift = 2)

# f follows Pareto distribution with alpha=2 and beta=3 
f <- dpareto(sample1, alpha, beta)

# g follows exponential density with parameter equals to 1, shifted by two to the right
g <- d2exp(sample1, rate = 1, shift = 2) 

# calculate E(X)
mean(sample1 * f / g)

# calculate E(X^2)
mean((sample1^2) * f / g)

# Histograms of h(x)f(x)/g(x), h(x) = x
hist(sample1 * f / g, main = "Histogram of xf(x)/g(x)")

# Histograms of h(x)f(x)/g(x), h(x) = x^2
hist((sample1^2) * f / g, main = "Histogram of x^2f(x)/g(x)")

# Histogram of weights f(x)/g(x)
hist(f / g, main="Histogram of weights f(x)/g(x)")



## problem 2

# "helical valley" function
theta <- function(x1,x2) atan2(x2, x1)/(2*pi)
f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2 + x[2]^2) - 1)
  f3 <- x[3]
  return(f1^2 + f2^2 + f3^2)
}

# define values for x1, x2, x3
x1 <- seq(-25,25, length = 1000)
x2 <- seq(-25,25, length = 1000)
x3 <- seq(-25,25, length = 1000)

# x1 fixed; x2 and x3 vary
f23 <- outer(x2,             # this gives all possibilities of f(x2, x3)
             x3,             # while x1 is fixed at 1
             Vectorize(function(x2, x3){f(c(1, x2, x3))}))
# plot slice of f where x1 treated as constant value
image(f23, main = "x1 fixed: 2-d function of x2 and x3", axes = F)
contour(f23, add = T, drawlabels = F)

# x2 fixed; x1 and x3 vary
f13 <- outer(x1,             # this gives all possibilities of f(x1, x3)
             x3,             # while x2 is fixed at 1
             Vectorize(function(x1, x3){f(c(x1, 1, x3))}))
# # plot slice of f where x2 treated as constant value
image(f13, main = "x2 fixed: 2-d function of x1 and x3", axes = F)
contour(f13, add = T, drawlabels = F)

# x3 fixed; x1 and x2 vary
f12 <- outer(x1,             # this gives all possibilities of f(x1, x2)
             x2,             # while x3 is fixed at 1
             Vectorize(function(x1, x2){f(c(x1, x2, 1))}))
# # plot slice of f where x3 treated as constant value
image(f12, main = "x3 fixed: 2-d function of x1 and x2", axes = F)
contour(f12, add = T, drawlabels = F)

# starting point at (1,1,1)
optim(c(1, 1, 1), f)  # use optim() to find minimum of f
# minimum value using optim() with starting point (1,1,1)
optim(c(1, 1, 1), f)$value
nlm(f, c(1, 1, 1))  # use nlm() to find minimum of f
# minimum value using nlm() with starting point (1,1,1)
nlm(f, c(1, 1, 1))$minimum

# generate random starting values
x1 <- runif(10, -10, 10)
x2 <- runif(10, -10, 10)
x3 <- runif(10, -10, 10)

# Explore the possibility of multiple local minima with optim()
n <- 10
optimMin <- c()
for (i in 1:n) {
  result <- optim(c(x1[i], x2[i], x3[i]), f)
  optimMin <- c(optimMin, result$value)
}
optimMin

# Explore the possibility of multiple local minima with nlm()
n <- 10
nlmMin <- c()
for (i in 1:n) {
  result <- nlm(f, c(x1[i], x2[i], x3[i]))
  nlmMin <- c(nlmMin, result$minimum)
}
nlmMin



## problem 3

#### c
rm(list = ls())

set.seed(1)
n <- 100
beta0 <- 1
beta1 <- 2
sigma2 <- 6

x <- runif(n)
yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))

## parameters chose such that signal in data is moderately strong
## estimate divided by std error is ~ 3
mod <- lm(yComplete ~ x)
summary(mod)$coef

#get the criteria infomation about lm(yComplete ~ x)
beta0lm <- summary(lm(yComplete ~ x))$coef[1,1]
beta1lm <- summary(lm(yComplete ~ x))$coef[2,1]
sigmaSquarelm <- sum((yComplete - beta0lm - beta1lm * x)^2)/n
criteria <- c(beta0lm, beta1lm, sigmaSquarelm)


EM <- function(x, y, numPercent){

  # numPercent is the number of the percentage of the data that is censored
  tau <- sort(y, decreasing = TRUE)[numPercent]
  xUncensored <- x[y <= tau]
  yUncensored <- y[y <= tau]
  xCensored <- x[y > tau]
  yCensored <- y[y > tau]
  # n is total observations
  n <- length(y)
  # c is censored observations
  c <- length(yCensored)
  
  # initial starting values based on part(b)
  mod <- lm(yUncensored ~ xUncensored)
  beta0 <- summary(mod)$coef[1,1]
  beta1 <- summary(mod)$coef[2,1]
  sigmaSquare <- sum((yUncensored - beta0 - beta1 * xUncensored)^2)/(n - c -2)
  
  # iternation
  i <- 1
  # error between current value and previous value
  error <- 1
  while (error > 0.00001){
    
    # value used to calculate error
    beta00 <- beta0
    beta10 <- beta1
    sigmaSquare0 <- sigmaSquare
    
    # According to the the Johnson and Kotz bibles on distributions
    muCensored <- beta0 + beta1 * xCensored
    tauStar <- (tau - muCensored)/sqrt(sigmaSquare) 
    rho <- dnorm(tauStar)/(1-pnorm(tauStar))
    mExpectation <- muCensored + sqrt(sigmaSquare)* rho
    mVariance <- sigmaSquare * (1+ tauStar*rho - rho^2)
    
    # rearrange to obtain xStar
    xStar <- c(xCensored, xUncensored)
    # add estimated value into yUncersored to obtain yStar
    yStar <- c(mExpectation, yUncensored)
    
    # obtain the estimations
    modStar <- lm(yStar ~ xStar)
    beta1 <- summary(modStar)$coef[2,1]
    beta0 <- summary(modStar)$coef[1,1]
    sigmaSquare <- (sum(summary(modStar)$residuals^2)  + sum(mVariance))/100
    
    # stop the optimization if the sum of the sqared value of "error" of the 
    # three parameters is smaller than 0.00001
    error <- ((beta00-beta0)^2 + (beta10-beta1)^2 + (sigmaSquare0-sigmaSquare)^2)
    i = i + 1
  }
  return(list(Beta0 = beta0, Beta1 = beta1, SigmaSquare = sigmaSquare))
}

EM(x, yComplete, 20)
EM(x, yComplete, 80)
criteria


