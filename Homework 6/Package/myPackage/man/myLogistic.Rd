\name{lr}
\alias{myLogistic}
\title{Logistic Regression}
\usage{
myLogistic(X, Y)
}
\description{
Implements logistic regression using QR Decomposition

Perform the logistic regression of Y on X
Input: 
X is an n x p matrix of explanatory variables
Y is an n dimensional vector of binary responses

myLogistic calls QR decomposition in turn
}
\examples{
n<-100
p<-5
X    <- matrix(rnorm(n * p), nrow = n)
Y <- round(matrix(runif(n), n, 1))
myLogistic(X,Y)

Example Output : 
[1]  0.17708078 -0.09499217 -0.19821952  0.02095166  0.11797965
}