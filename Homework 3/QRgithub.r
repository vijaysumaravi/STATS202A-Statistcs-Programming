#########################################################
## Stat 202A - Homework 6
## Author:
## Date :
## Description: This script implements QR decomposition
## and linear regression based on QR
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

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){

  ## Perform QR factorization on the matrix A
  ## FILL IN CODE HERE ##

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
  return(list("Q" = t(Q), "R" = R))

}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLM <- function(X, Y){

  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function

  ## FILL CODE HERE ##
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(matrix(1, n, 1), X, matrix(Y, n, 1))

  result = myQR(Z)
  R = result$R

  R1 = R[1:(p+1),1:(p+1)]
  Y1 = R[1:(p+1), p+2]
  beta_ls = solve(R1, Y1)

  ## Function returns beta_ls, the least squares
  ## solution vector
  return(beta_ls)
}

###
# OUTPUT
###

# Test myLM
# n = 100
# p = 5
# X = matrix(runif(n * p), nrow = n)
# beta = 1 : p
# Y = X %*% beta + rnorm(n)
# print(myLM(X,Y))

# test myQR
# A = matrix(runif(25), 5, 5)
# print(myQR(A))
# print(myQRC(A))

# Plot times
for(i in c(25, 75, 125, 175, 225, 275)){
  print(paste0("Execution time for N= ", i))

  # Time myQR
  A = matrix(runif(i * i), nrow=i)
  startTime = proc.time()
  mySweep(A, i)
  stopTime = proc.time()
  print("R")
  print(stopTime - startTime)

  # Time myQRC
  startTime = proc.time()
  mySweepC(A, i)
  stopTime = proc.time()
  print("Cpp-> ")
  print(stopTime - startTime)
  cat()
}
