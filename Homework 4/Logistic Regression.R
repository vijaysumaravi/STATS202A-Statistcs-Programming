#########################################################
## Stat 202A - Homework 4
## Author: 
## Date : 
## Description: This script implements logistic regression
## using iterated reweighted least squares using the code 
## we have written for linear regression based on QR 
## decomposition
#########################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################


################ Optional ################ 
## If you are using functions from Rcpp, 
## uncomment and adjust the following two 
## lines accordingly. Make sure that your 
## cpp file is in the current working
## directory, so you do not have to specify
## any directories in sourceCpp(). For 
## example, do not write
## sourceCpp('John_Doe/Homework/Stats202A/QR.cpp')
## since I will not be able to run this.

## library(Rcpp)
## sourceCpp(name_of_cpp_file)

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){
  
  ## Perform QR decomposition on the matrix A
  ## Input: 
  ## A, an n x m matrix
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n <- nrow(A)
  m <- ncol(A)
  Q <- diag(n)
  R <- A
  
  for(k in 1:(m - 1)){
    x      <- rep(0, n)
    x[k:n] <- R[k:n, k]
    s      <- -1 * sign(x[k])
    v      <- x
    v[k]   <- x[k] - s * norm(x, type = "2")
    u      <- v / norm(v, type = "2")
    
    R <- R - 2 * u %*% t(u) %*% R
    Q <- Q - 2 * u %*% t(u) %*% Q
    
  }
  
  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))
  
}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLM <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n <- nrow(X)
  p <- ncol(X)
  
  ## Stack (X, Y) and solve it by QR decomposition
  Z <- cbind(X, Y)
  R <- myQR(Z)$R
  
  R1 <- R[1:(p + 0), 1:(p + 0)]
  Y1 <- R[1:(p + 0), p + 1]
  
  beta_ls <- solve(R1) %*% Y1
  
  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogistic <- function(X, Y){

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  
  r <- nrow(X)
  p <- ncol(X)

  beta    <- rep(0, p)
  epsilon <- 1e-6
  
  numIter <- 1
  while(TRUE && numIter < 100){
    eta <- X %*% beta
    pr  <- (1 / (1 + exp(-eta)))
    w   <- pr * (1 - pr)
    z   <- eta + (Y - pr) / w
    sw  <- sqrt(w)
    mw  <- matrix(rep(sw, times = p), ncol = p)
    
    x_work <- mw * X
    y_work <- sw * z
    
    beta_new <- as.vector(coef(lm(y_work ~ x_work)))[2:5]
    err      <- sum(abs(beta_new - beta))
    beta     <- beta_new
    if(err < epsilon)
      break
    numIter <- numIter + 1
  }
  
  beta  
    
}


## Simulation
n <- 5000
p <- 4

X    <- matrix(rnorm(n * p), nrow = n)
beta <- c(12, -2,-3, 4)
Y    <- 1 * (runif(n) < (1 / (1 + exp(-(X %*% beta))))

## Our solution
logistic_beta <- myLogistic(X, Y)
logistic_beta    

## R's solution
coef(glm(Y ~ X + 0, family = binomial(link = 'logit')))