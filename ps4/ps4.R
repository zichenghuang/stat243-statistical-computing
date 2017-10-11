#########################################
####  Zicheng Huang
####  STAT243 PS4
#########################################


library(ggplot2)
library(microbenchmark)



#1(a)
x <- 1:10
f <- function(input){
  data <- input
  .Internal(inspect(data))
  .Internal(inspect(input))
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
# According to the result from .Internal(inspect), we can see that only one
# copy of the vector 1:10 is created, the two local variables input and data
# are pointing to the same copy of the vector 1:10 since their addresses are
# the same.


#1(b)
x <- 1:10000
length(serialize(x, NULL))
f <- function(input){
  data <- input
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
length(serialize(myFun, NULL))
# We make x a large enough vector with size of around 40000 bytes. Since in
# question 1(a) we argue that only one copy of x is made, we expect to see
# the number of bytes that store the information in the closure to be close
# to the size of x, which is around 40000 bytes. However, the actual size of
# the serialized myFun is around 80000 bytes, which is twice the size of what
# we expect. This difference is due to the fact that under the function frame
# of myFun there exist the body text of the function, the variable 'input', 
# and the variable 'data'. Even though both 'input' and 'data' are pointing to
# the same sequence of numbers, i.e. 1:10000, when R save the function myFun
# what happens is that 'input' and 'data' are saved seperated as two individual
# objects each takes up around 40000 bytes of size. Thus, the size of myFun is
# 80000 bytes which is twice the size of what we expected. 


#1(c)
x <- 1:10
f <- function(data){
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
rm(x)
data <- 100
myFun(3)
# When myFUn is define by passing in f(x), f(x) is not evaluated due to the 
# lazy evaluation. When myFun(3) is called, we see that what is actually
# being called is g(3). When g(3) is evaluated, g will look for a variable
# 'data' to return (3 * data), but since no 'data' is defined in the function
# frame of g, it will go to the enclosing environment, function frame of f,
# to look for 'data'. When we define myFun, we pass in f(x) to myFun, so the
# argument 'data' of f should taks in x, but since x is removed, g cannot
# find 'data' in the function frame of f as well. Since there exists no
# 'data' in the global environment, the call on myFun(3) will return error.


#1(d)
x <- 1:10000
f <- function(data){
  force(data) # add this line to prevent the lazy evaluation from happening
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
rm(x)
myFun(3)
length(serialize(myFun, NULL))
# Since we prevented the lazy evaluation from happening, when we define
# myFun by passing in f(x), f(x) will be evaluated in a sense that argument
# 'data' will take in the user supplied variable of 'x'. In this way, even
# 'x' is removed, when myFun(3) is called, g(3) will be able to find a local
# vaiable 'data' in the function frame of f to return (param * data), thus
# preventing the error from happening again. 
# The resulting serialized closure has size around 40000 bytes


#2(a)
lst <- list(c(1, 2),c(3, 4))
.Internal(inspect(lst))
lst[[1]][1] <- 8
.Internal(inspect(lst))
# We see this in R session, not Rstudio.
# We can see that the address of the two lists, before and after modification,
# are the same, so no new lists are created when the change is made.


#2(b)
lst = list(c(1, 2),c(3, 4))
lstCopy <- lst
.Internal(inspect(lst))
.Internal(inspect(lstCopy))
# After making a copy of the original list 'lst', we see that there is no
# copy-on-change going on since the address of the two lists are the same

.Internal(inspect(lst))
lst[[1]][1] <- 8
.Internal(inspect(lst))
# A copy of the entire list is made in a sense that the entire vectors of the addresses that stored the two vectores contained in the original list is copied. The sub-vector that is modified is also copied in order to make the modification.


#2(c)
# After adding an element to lstC, the vectors that stored the addresses of the two original sublists are copied, but the address of each of the original sublist itself is not copied. The data in the sublists stored in the two uncopied addresses are shared.

lst1 <- list(1, 2)
lst2 <- list(3, 4)
lst <- list(lst1, lst2)
lstC <- lst
.Internal(inspect(lst))
.Internal(inspect(lstC))
lstC[[3]] <- list(5, 6)
.Internal(inspect(lst))
.Internal(inspect(lstC))


#2(d)
gc()
tmp <- list()
x <- rnorm(1e7)
tmp[[1]] <- x
tmp[[2]] <- x
.Internal(inspect(tmp))
object.size(tmp)
gc()


#3
load('ps4prob3.Rda') # should have A, n, K
ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}

oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) {
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  # compress the three nested for loops to one for loop doing what is essentially
  # the matrix multiplication
  for (i in 1:K) {
    q[ , , i] <- theta.old[, i] %*% t(theta.old[, i]) / Theta.old
  }
  theta.new <- theta.old
  for (z in 1:K) {
    # create a variable B to prevent from calculating A*q[,,z] twice in every 
    # for loop for K times
    B = A*q[,,z]
    theta.new[,z] <- rowSums(B)/sqrt(sum(B))
  }
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check))
}
# initialize the parameters at random starting values
temp <- matrix(runif(n*K), n, K)
theta.init <- temp/rowSums(temp)
# do single update
out <- oneUpdate(A, n, K, theta.init)
# in the real code, oneUpdate was called repeatedly in a while loop
# as part of an iterative optimization to find a maximum likelihood estimator

system.time(out <- oneUpdate(A, n, K, theta.init))


#4

# first algorithm
PIKK <- function(x, k) {
  x[sort(runif(length(x)), index.return = TRUE)$ix[1:k]]
}

# Use runif() to generate 2k numbers between 0 and 1.
# Multiply these 2k numbers with the length(x) to obtain 2k numbers ranging from 0 to the length(x).
# These 2k numbers now have decimal places, use round() to convert them to integers. 
# Use unique() to filter and keep the unique numbers. This step makes sure the characteristic of without replacement.
# Select the first k numbers in the sequences of unique numbers we just obtain.
# Return these k numbers as sample without replacement.
modPIKK <- function(x, k) {
  x[unique(round(runif(2*k)*length(x)))[1:k]]
}

# different length of x
n <- c(5000, 6000, 7000, 8000, 9000, 10000)
# time for original PIKK
timePIKK <- c()
# time for modified PIKK
timePIKKmod <- c()

# obtain time for original PIKK
for (i in n) {
  x <- rnorm(i)
  k <- i/20
  timePIKK <- c(timePIKK, mean(microbenchmark(PIKK(x,k))$time))
}

# obtain time for modified PIKK
for (i in n) {
  x <- rnorm(i)
  k <- i/20
  timePIKKmod <- c(timePIKKmod, mean(microbenchmark(modPIKK(x,k))$time))
}



# second algorithm
FYKD <- function(x, k) {
  n <- length(x)
  for(i in 1:n) {
    j = sample(i:n, 1)
    tmp <- x[i]
    x[i] <- x[j]
    x[j] <- tmp
  }
  return(x[1:k])
}

# modified FYKD by only swapping the first k numbers in x,
# instead of swapping all numbers in x. Then select the first
# k numbers.
modFYKD <- function(x, k) {
  n <- length(x)
  # instead of 1:n, use 1:k
  for(i in 1:k) {
    j = sample(i:n, 1)
    tmp <- x[i]
    x[i] <- x[j]
    x[j] <- tmp
  }
  return(x[1:k])
}

# time for original FYKD
timeFYKD <- c()
# time for modified FYKD
timeFYKDmod <- c()

# obtain time for original FYKD
for (i in n) {
  x <- rnorm(i)
  k <- i/20
  timeFYKD <- c(timeFYKD, mean(microbenchmark(FYKD(x,k))$time))
}

# obtain time for modified FYKD
for (i in n) {
  x <- rnorm(i)
  k <- i/20
  timeFYKDmod <- c(timeFYKDmod, mean(microbenchmark(modFYKD(x,k))$time))
}


df <- data.frame(number = n, PIKKtime = timePIKK, 
                 modPIKKtime = timePIKKmod, FYKDtime = timeFYKD, 
                 modFYKDtime = timeFYKDmod)

## plot comparison for first algorithm
g1 <- ggplot(df) + 
  geom_line(aes(x = number, y = PIKKtime)) +
  geom_line(aes(x = number, y = modPIKKtime)) +
  xlab("different length of x") + 
  ylab("nanoseconds")

## plot comparison for second algorithm
g2 <- ggplot(df) + 
  geom_line(aes(x = number, y = FYKDtime)) +
  geom_line(aes(x = number, y = modFYKDtime)) +
  xlab("different length of x") + 
  ylab("nanoseconds")

## PIKK
g1
## FYKD
g2
