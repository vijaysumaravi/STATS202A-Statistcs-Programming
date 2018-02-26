"
Name: Salil Kanetkar
UCLA ID: 704557096
The data set has been directly imported from the URL. Hence while running the code,
it is necessary that the internet is connected to.
"

expit <- function(x)
{
  n = nrow(x)
  p = ncol(x)
  y=matrix(nrow=n,ncol=p)
  for(i in 1:n)
    for(j in 1:p)
      y[i,j] <- 1/(1+exp(-x[i,j]))
  return(y)
}

mylm <- function(X,Y){
  
  n=nrow(X)
  p=ncol(X)
  Z = cbind(X, Y)
  R = myqr(Z)$R
  R1 = R[1:(p), 1:(p)]
  Y1 = R[1:(p), p+1]
  beta = mysolve(R1, Y1)
  return(beta)
}

myqr <- function(A)
{
  n=nrow(A)
  p=ncol(A)
  Q=matrix(nrow=n,ncol=p)
  R=matrix(nrow=n,ncol=p)
  U=matrix(0,nrow=n,ncol=p)
  U[,1]=A[,1]
  for (k in 2:p)
  {
    proj=matrix(0,nrow=n,ncol=1)
    for(i in 1:(k-1))
    {
      a=sum(A[,k]*U[,i])
      b=sum(U[,i]*U[,i])
      c=a/b
      ck=c*U[,i]
      proj = proj + ck
    }  
    U[,k]=A[,k]-proj[,1]
  }
  sq = matrix(0,n,1)
  temp=matrix(0,nrow=n,ncol=1)
  Q=matrix(0,nrow=n,ncol=p)
  for(i in 1:p)
  {
    temp=U[,i]*U[,i]
    sq[i,1]=sqrt(sum(temp))
    Q[,i]=U[,i]/sq[i,1]
  }
  R= t(Q) %*% A
  result <- list(Q, R)
  names(result) <- c("Q", "R")
  result
}

mysolve <- function(X,Y){
  n = nrow(X)
  p = ncol(X)
  X1 <- cbind(X[1:p,1:p],Y)
  X2 <- cbind(t(Y),1)
  XX <- rbind(X1,X2)
  S = mySweep(XX,p)
  S1=S[1:p,(p+1)]
  return(S1)
}

mySweep <- function(A, m){
  n <- dim(A)[1]
  for (k in 1:m) 
  {
    for (i in 1:n)     
      for (j in 1:n)   
        if (i!=k  & j!=k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for (i in 1:n) 
          if (i!=k) 
            A[i,k] <- A[i,k]/A[k,k]  
          
          for (j in 1:n) 
            if (j!=k) 
              A[k,j] <- A[k,j]/A[k,k] 
            A[k,k] <- - 1/A[k,k] 
  }
  return(A)
}

mylogistic <- function(X, Y)
{
  n <- nrow(X)
  p <- ncol(X)
  beta <- matrix(rep(0, p), nrow = p)
  epsilon <- 1e-6
  repeat
  {
    eta <- X%*%beta
    pr <- expit(eta)
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    Xwork <- mw*X
    Ywork <- sw*Z
    beta_new <- mylm(Xwork,Ywork)
    err <- sum(abs(beta_new-beta))
    beta <- beta_new
    if (err<epsilon)
      break
  }
  return(beta)
}

binary <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata1 = as.matrix(binary)
n=nrow(mydata1)
p=ncol(mydata1)
intercept = matrix(1,nrow=n,ncol=1)
colnames(intercept) <- c("intercept")
mydata2 = cbind(intercept,mydata1)
rank_matrix = matrix(0,nrow=n,ncol=3)
colnames(rank_matrix) <- c("rank2","rank3","rank4")
mydata = cbind(mydata2,rank_matrix)
admit = mydata[,2]
for (i in 1:n)
{
  if(mydata[i,5] == 2)
    mydata[i,6]=1
  if(mydata[i,5] == 3)
    mydata[i,7]=1
  if(mydata[i,5] == 4)
    mydata[i,8]=1
}
mydata=mydata[,-2]
mydata=mydata[,-4]
beta = mylogistic(mydata,admit)
print(beta)