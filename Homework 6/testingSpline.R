library(Rcpp)

sourceCpp("sweep.cpp")

error <- function(Y,Yhat){
  return(mean((Y-Yhat)^2))
} 

test_spline <- function(x,Y,beta){
  p = length(beta)-1
  n = length(x)
  X = matrix(x, nrow = n)
  
  for (k in (1:(p-1))/p)
    X = cbind(X, (x>k)*(x-k))
  Yhat = cbind(rep(1,dim(X)[1]), X)%*%beta
  return(Yhat)
}

my_ridge <- function(X, Y, lambda)
{
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweepC(A, p+1)
  beta = S[1:(p+1), p+2]
  return(beta)
}


my_spline <- function(x,Y,lambda = 10.,p) {
  n = length(x)
  X = matrix(x, nrow=n)
  for (k in (1:(p-1))/p)
    X = cbind(X, (x>k)*(x-k))
  
  beta = my_ridge(X, Y, lambda)
  return(beta)
}

n = 100
p = 500
sigma = .1

x = runif(n)
Y = x^2 + rnorm(n)*sigma
X = matrix(x, nrow=n)
arr = c(50,25,100,10,1,0.5,0.1,0.01, 0.001, 0.0001)
# ### Divinding datasets into train(2/3 of data) and test(2/3 of data)
# no_train = round(2*n/3)
# no_test = n - no_train
# train_ind =sample(1:n,no_train)
# test_ind = setdiff(1:n,train_ind)

# X_train = sort(x[train_ind])
# #X_train = sort(runif(no_train))
# Y_train = X_train^2 + rnorm(no_train)*sigma

# X_test = sort(x[test_ind])
# #X_test = sort(runif(no_test))
# Y_test = X_test^2 + rnorm(no_test)*sigma

# Y_train = Y[train_ind]
# Y_test = Y[test_ind]

# training_error = NULL
# testing_error = NULL

for(lambda_it in arr){
  beta = my_spline(x,Y,lambda_it,p)
  Yhat = test_spline(x,Y,beta)
  index = order(x)
  x = x[index]
  Y = Y[index]
  Yhat = Yhat[index]
  plot(x, Y, ylim = c(-.2, 1.2), col = "red", type="p", main=paste(" Plot when lambda is",lambda_it))
  par(new = TRUE)
  plot(x, Yhat, ylim = c(-.2, 1.2), col = "green", type="l", ylab="Y")
  legend(10,10,legend=c("Observed points","Fitted curve"))
}

# lambda_it = seq(1,100)
# par(mfrow = c(1,2))
# plot(lambda_it, training_error, col = "red")
# plot(lambda_it, testing_error, col = "green")


