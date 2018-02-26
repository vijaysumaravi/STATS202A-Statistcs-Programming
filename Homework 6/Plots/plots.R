library(Rcpp)
sourceCpp("sweep.cpp")

myRidgeCpp <- function(X, Y, lambda)
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

error <- function(Yhat,Y)
{
  return (mean((Yhat-Y)^2))
}

my_spline <- function(X_train, Y_train, X_test, Y_test, lambda)
{
  beta = myRidgeCpp(X_train, Y_train, lambda)
  Yhat_train = cbind(rep(1, n_train), X_train)%*%beta
  Yhat_test = cbind(rep(1, n_test), X_test)%*%beta
  Y_total = list(Yhat_train,Yhat_test)
  return (Y_total)
}







  n = 100
  n_train = ceiling(2*n/3)
  n_test = n-n_train
  p = 500
  sigma = .1
  lambda = 10.
  x = runif(n)
  
  train_ind =sample(1:n,n_train)
  test_ind = setdiff(1:n,train_ind)
  
  #x_train1 = sample(x, size = n_train, replace = FALSE)
  #x_train = sort(x_train1)
  x_train = sort(x[train_ind])
  y_train = x_train^2 + rnorm(n_train)*sigma
  Y_train = matrix(y_train, nrow=n_train)
  x_test = sort(x[test_ind])
  #x_test1 = setdiff(x,x_train1)
  #x_test = sort(x_test1)
  y_test = x_test^2 + rnorm(n_test)*sigma
  Y_test = matrix(y_test, nrow=n_test)
  X_train = matrix(x_train, nrow=n_train)
  X_test = matrix(x_test, nrow=n_test)
  error_test = NULL
  error_train = NULL
  for (k in (1:(p-1))/p){
    X_train = cbind(X_train, (x_train>k)*(x_train-k))
    X_test = cbind(X_test, (x_test>k)*(x_test-k))
  }
  Y_total=NULL
  for(lambda in 1:120){
    Y_total=my_spline(X_train, Y_train, X_test, Y_test, lambda)
    Yhat_train = Y_total[[1]]
    Yhat_test = Y_total[[2]]
    error_train = append(error_train,error(Yhat_train,Y_train))
    error_test = append(error_test,error(Yhat_test,Y_test))
  }
  lambda1 = 1:120
  
  plot(lambda1,error_train,col="red",xlab="Lambda",ylab="Training Error",type="p")
  
  plot(lambda1,error_test,col="green",xlab="Lambda",ylab="Testing Error",type="p")