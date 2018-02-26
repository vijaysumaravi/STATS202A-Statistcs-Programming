#########################################################
## Stat 202A - Homework 6
## Author: 
## Date : November 16th, 2017
## Description: This script implements linear regression
## and logistic regression and returns their 
## standard errors
#########################################################


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
  
  n = dim(A)[1]
  m = dim(A)[2]
  
  R = A
  Q = diag(n)
  
  for(k in 1 : (m - 1)){
    x = matrix(0, n, 1)
    x[k:n, 1] = R[k:n, k]
    v = x
    
    v[k] = x[k] + sign(x[k,1]) * norm(x, type="F")
    s = norm(v, "F")
    
    if(s != 0){
      u = v / s
      R = R - 2 * (u %*% (t(u) %*% R))
      Q = Q - 2 * (u %*% (t(u) %*% Q))
    }
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
  ## Use myQR (or myQRC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n=nrow(X)
  p=ncol(X)
  Z = cbind(rep(1,n),X, Y)
  R = myQR(Z)$R
  R1 = R[1:(p+1), 1:(p+1)]
  Y1 = R[1:(p+1), p+2]
  
  beta_ls = solve(R1, Y1)
  
  std_error <- calc_std_error_LM(X, Y, beta_ls)
  
  return(list("coefficients" = beta_ls, "standard_error" = std_error))

  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogistic <- function(X, Y){
  
  ## Perform the logistic regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################
  
  n = dim(X)[1]
  p = dim(X)[2]
  
  beta = matrix(rep(0,p), nrow = p)
  
  epsilon = 1e-6
  
  repeat
  {
    X_beta_product = X %*% beta
    pr = expit(X_beta_product)
    z = X_beta_product + (Y - pr)/(pr*(1-pr))
    sqw = sqrt(pr*(1-pr))
    mw = matrix(sqw, n, p)
    xw = mw*X
    yw = sqw*z
    
    beta_n = myLM_logistic(xw, yw)
    error = sum(abs(beta_n - beta))
    beta = beta_n
    if (error < epsilon)
      break
    
    
    
  }
  
  
  std_error = calc_std_error_logistic(X, beta)
  ## Function returns the logistic regression solution vector
  return(list("coefficients" = beta, "standard_error" = std_error))
  
}

######################################################
## Function 4: Standard Error Calculation for LinearReg##
######################################################

calc_std_error_LM <- function(X, Y, my_coef){
  
  # Find the standard error of Linear Regression
  
  X_bias = cbind(rep(1,n),X)
  XtX_inv = solve(t(X_bias) %*% X_bias)
  
  RSS =  sum(((X_bias %*% matrix(my_coef)) - Y)^2)
  sigma_squared = RSS/(n-p-1)
  
  std_error = sqrt(diag(sigma_squared * XtX_inv))
  
  return(std_error)
  
  
  
}


######################################################
## Function 5: Standard Error Calculation for Logistic##
######################################################

calc_std_error_logistic <- function(X, my_coef){
  
  # Find the standard error of Logistic Regression
  
  X_beta_product = X %*% my_coef
  pr = expit(X_beta_product)
  w = diag(c(pr*(1-pr)))
  
  std_error = solve((t(X) %*% w) %*% X)
  
  return(sqrt(diag(std_error)))
  
  
}

###############################################
## Function 6: Linear regression based on QR ##
###############################################

myLM_logistic <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR (or myQRC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n=nrow(X)
  p=ncol(X)
  Z = cbind(X, Y)
  R = myQR(Z)$R
  R1 = R[1:(p), 1:(p)]
  Y1 = R[1:(p), p+1]
  
  beta_ls = solve(R1, Y1)
  
  
  
  ## Function returns the least squares solution vector
  return(beta_ls)
  
}

