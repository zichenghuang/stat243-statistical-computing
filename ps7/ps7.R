#### Zicheng Huang
#### STAT243 ps7


##4(b)
estimateBetaHat <- function(A,X,Y,b){
  # QR decompose X
  R <- qr.R(qr(X))
  Q <- qr.Q(qr(X))
  # QR decompose (AR^{-1})^T
  r <- qr.R(qr(t(A %*% solve(R))))
  # solve for betahat using algorithm mentioned in the pseudo-code
  # step 3
  backsolve(crossprod(R),
                                   # step 2
            t(X) %*% Y + t(A) %*% (backsolve(crossprod(r),
                                                     # step 1
                                             -A %*% (backsolve(R, t(Q) %*% Y)) + b)))
}


#### 6
set.seed(1)
#### create a set of eigenvectors

## create arbitrary Z
Z <- matrix(rnorm(100*100), nrow = 100, ncol = 100)
## Compute A as cross product of Z, then A is symmetric
A <- crossprod(Z)
## eigendecompose A to obtain its eigenvectors
eigenVectorsA <- eigen(A)$vectors

#### Now we want to compare the computed eigenvalues and the actual eigenvalues
#### First we start with eigenvalues all being equal

## create a matrix with eigenvalues that are all the same, with value 3
eigenValues0 <- diag(x = rep(3, 100), nrow = 100, ncol = 100)

## numerical calculate the computed eigenvalues
computedEigenValues0 <- eigen(eigenVectorsA %*% eigenValues0 %*% t(eigenVectorsA))$values

## difference between computed eigenvalues and actual eigenvalues
computedEigenValues0 - diag(eigenValues0)


#### Now we generate eigenvalues vary from a range of values from very large to very small
## loop index
i <- 1
## min set to 1 to start the loop
min <- 1
## vector storing condition numbers
condNum <- c()
## vector storing errors
error <- c()
## use the eigen vectors obtained from matrix A
eigenVectors <- eigenVectorsA
## the loop will break when then min of the computed eigenvalues is less than 0, 
## this means that we get a matrix that is not numerically positive definite
while (min > 0){
  set.seed(666)
  ## generate eigen values with different magnitudes
  eigenValues <- diag(x = sort(seq(from = 0.001, to = (10^i), by = ((10^i)-0.001)/99), decreasing = T),
                      nrow = 100, ncol = 100)
  ## numerically calculate the computed eigen values
  computedEigenValues <- eigen(eigenVectors %*% eigenValues %*% t(eigenVectors))$values
  ## compute the condition number
  condNum <- c(condNum, max(computedEigenValues)/min(computedEigenValues))
  ## compute the error by median of the absolute difference
  error <- c(error, median(abs(computedEigenValues - diag(eigenValues))))
  ## set min equals the minimum of the computed eigenvalues
  min = min(computedEigenValues)
  i = i + 1
}

## at this condition number we empirically see that the matrix is not numerically positive definite
abs(condNum[i-1])

error 

library(ggplot2)
## plot to see the relationship between the error and the condition number
## Index measures the order of magnitude, starting at 4
## Error measures the median of absolute difference
## logError measures the log of Error
df <- data.frame(Index = 4:(i-1+3), Error = error, logError = log(error), CondNum = abs(condNum[1:i-1]))

g1 <- ggplot(df) +
  geom_line(aes(x = Index, y = Error)) +
  ggtitle("Error VS. Order of Magnitude of Eigenvalues") + 
  xlab("Order of Magnitude of Eigenvalues")
g1

g2 <- ggplot(df) +
  geom_line(aes(x = Index, y = logError)) +
  ggtitle("log Error VS. Order of Magnitude of Eigenvalues") + 
  xlab("Order of Magnitude of Eigenvalues")
g2

g3 <- ggplot(df) +
  geom_line(aes(x = CondNum, y = Error)) +
  ggtitle("Error VS. Condition Number") + 
  xlab("Order of Magnitude of Eigenvalues")
g3

g4 <- ggplot(df) +
  geom_line(aes(x = CondNum, y = logError)) +
  ggtitle("log Error VS. Condition Number") + 
  xlab("Order of Magnitude of Eigenvalues")
g4






