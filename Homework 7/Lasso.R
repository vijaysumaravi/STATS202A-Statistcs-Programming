############################################################# 
## Stat 202A - Homework 7
## Author: 
## Date : 
## Description: This script implements the lasso
#############################################################

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

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector 
  # beta for each regularization parameter.
 
  #######################
  ## FILL IN CODE HERE ##
  #######################
  lambda_all = sort(lambda_all, decreasing=TRUE)
  
  # Parameters
  p = dim(X)[2]
  n = dim(X)[1]
  L = length(lambda_all)
  X = cbind(rep(1,n),X)
  # Constants
  T = 10
  
  # Beta
  beta = matrix(rep(0, p+1), nrow = p+1)
  beta_all = matrix(rep(0, (p+1)* L), nrow = p+1)

  R = Y
  ss = rep(0, p+1)
  for(j in 1 : p+1){
    ss[j] = sum(X[, j]^2)
    print(ss[j])
  }

  for (l in 1:L){
    lambda = lambda_all[l]
    for (t in 1:T){
      for (j in 2:p+1){
        db = sum(R * X[ , j]) / ss[j]
        b = beta[j] + db
        b = sign(b) * max(0, abs(b) - lambda / ss[j])
        db = b - beta[j]
        R = R - X[ , j] * db
        beta[j] = b
      }
    }
    beta_all[ , l] = beta
  }  
  
  
  ## Function should output the matrix beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)
  
}


  
  x=matrix(rnorm(50*200),50,200)
  y=rnorm(50)
  lambda_all = (0.1:100)/10
  beta_all = myLasso(x,y,lambda_all)


