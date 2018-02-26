#########################################################
## Stat 202A - Homework 6
## Author: V
## Date : 11/09/2017
## Description: This script implements logistic regression, 
## Linear Regression, PCA using QR decomposition
#########################################################


#' QR Decomposition
#'
#' Performs QR decomposition on the matrix A.
#' Function should output a list with Q.transpose and R
#' Q is an orthogonal n x n matrix
#' R is an upper triangular n x m matrix
#' Q and R satisfy the equation: A = Q %*% R
#' @param A->
#' an n x m matrix
#' @export
#' @examples
#' myQR(A)

myQR <- function(A){
  
 
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

#' Linear Regression
#'
#' Performs the linear regression of Y on X. 
#' Function returns the 1 x (p + 1) vector beta_ls, the least squares solution vector.
#' @param X->
#'  X is an n x p matrix of explanatory variables
#' @param Y->
#'  Y is an n dimensional vector of responses
#' @export
#' @examples
#'  X    <- matrix(rnorm(n * p), nrow = n)
#'  beta <- matrix(1:p, nrow = p)
#'  Y    <- X %*% beta + rnorm(n)
#'  myLM(X,Y)

myLM <- function(X, Y){
  
  ## Performs the linear regression of Y on X
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
# expit <- function(x){
#   1 / (1 + exp(-x))
# }

#' Logistic Regression
#'
#' Performs the logistic regression of Y on X
#' using iterated reweighted least squares obtained from linear regression based on QR decomposition.
#' Function returns the logistic regression solution vector.
#' @param X -> is an n x p matrix of explanatory variables
#' @param Y -> is an n dimensional vector of binary responses
#' @examples
#' X    <- matrix(rnorm(n * p), nrow = n)
#' beta <- matrix(rep(0, p), nrow = p)
#' Y    <- 1 * (runif(n) < (1 / (1 + exp(X %*% beta))))
#' logistic_beta <- myLogistic(X, Y)

myLogistic <- function(X, Y){

  ## Perform the logistic regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(X)
  p <- ncol(X)
  beta <- matrix(rep(0, p), nrow = p)
  epsilon <- 1e-8
  repeat
  {
    eta <- X%*%beta
    pr <- (1 / (1 + exp(-eta)))
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    Xwork <- mw*X
    Ywork <- sw*Z
    beta_new <- myLM(Xwork,Ywork)
    beta_new <- beta_new[0:-1]
    err <- sum(abs(beta_new-beta))
    beta <- beta_new
    if (err<epsilon)
      break
  }
  return(beta)
  
  
  ## Function returns the logistic regression solution vector
  beta  
    
}

#' Principle Component Analysis
#'
#' Performs PCA on matrix A using  QR Decomposition .
#' Function should output a list with D and V. 
#' D is a vector of eigenvalues of A. 
#' V is the matrix of eigenvectors of A (in the 
#' same order as the eigenvalues in D.)
#' @param A-> Square matrix
#' @param numIter-> Number of iterations
#' @export
#' @examples
#' X = matrix(runif(n*p), nrow = n)
#' n = dim(X)[1]
#' p = dim(X)[2]
#' output_qr = myEigen_QR(t(X) %*% X, 1000)


myEigen_QR <- function(A, numIter = 1000){
  
  ## Perform PCA on matrix A using your QR function, myQR
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  r <- nrow(A)
  c <- ncol(A)
  V <- matrix(runif(r * r), r, r)
  
  for(i in 1:numIter){
    Q <- myQR(V)$Q
    V <- A %*% Q
  }
  
  eigen_out <- myQR(V)
  Q <- eigen_out$Q
  R <- eigen_out$R
  
  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the 
  ## same order as the eigenvalues in D.)
  return(list("D" = diag(R), "V" = Q))
  
}