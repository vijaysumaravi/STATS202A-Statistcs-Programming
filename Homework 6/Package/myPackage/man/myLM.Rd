\name{lm}
\alias{myLM}
\title{Linear Regression}
\usage{
myLM(X, Y)
}
\description{
Implements linear regression using QR Decomposition

Perform the linear regression of Y on X
Input: 
X is an n x p matrix of explanatory variables
Y is an n dimensional vector of responses

myLM calls QR decomposition in turn
}
\examples{
n<-100
p<-5
X    <- matrix(rnorm(n * p), nrow = n)
beta <- matrix(1:p, nrow = p)
Y    <- X %*% beta + rnorm(n)
myLM(X,Y)

Example Output : 
[1] -0.109015734 -0.099721932 -0.033108515 -0.008818522  0.084045981
}