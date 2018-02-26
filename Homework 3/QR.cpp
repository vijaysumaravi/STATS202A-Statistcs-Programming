/*
#########################################################
## Stat 202A - Homework 3
## Author: Vijay Ravi
## Date : 10/26/2018
## Description: This script implements QR decomposition,
## linear regression, and eigen decomposition / PCA 
## based on QR.
#########################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
*/ 


# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Sign function for later use 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 1: QR decomposition 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */  
  

// [[Rcpp::export()]]
List myQRC(const mat A){ 
  
  /*
  Perform QR decomposition on the matrix A
  Input: 
  A, an n x m matrix (mat)

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */ 
  int n = A.n_rows;
  int m = A.n_cols;

  mat R = mat(A);
  mat Q = eye(n, n);

  for(int k = 0; k < m - 1; k++){
    mat x = zeros(n, 1);

    for(int l = k; l < n; l++){
      x(l,0) = R(l,k);
    }

    mat v = mat(x);
    v(k) = x(k) + signC(x(k,0)) * norm(x, "fro");
    double s = norm(v, "fro");
    if(s != 0){
      mat u = v / s;
      R = R - 2 * (u * (u.t() * R));
      Q = Q - 2 * (u * (u.t() * Q));
    }
  }
  /*
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
  }*/

  List output;

  // Return a list with two named objects, Q and R
  // Function should output a List 'output', with 
  // Q.transpose and R
  // Q is an orthogonal n x n matrix
  // R is an upper triangular n x m matrix
  // Q and R satisfy the equation: A = Q %*% R
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);
  

}
  
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 2: Linear regression using QR 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  
  
// [[Rcpp::export()]]
mat myLinearRegressionQRC(const mat X, const mat Y){
    
  /*  
  Perform the linear regression of Y on X
  Input: 
  X is an n x p matrix of explanatory variables
  Y is an n dimensional vector of responses
  Do NOT simulate data in this function. n and p
  should be determined by X.
  Use myQRC inside of this function
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */  
  int n = X.n_rows;
  int p = X.n_cols;
  mat W = ones<mat>(n,1);
  mat temp = join_rows(X,Y);
  mat Z = join_rows(W,temp);
  List result = myQRC(Z);
  mat R = result["R"];

  mat R1 = R(span(0,p),span(0,p));
  mat Y1 = R(span(0,p), p+1);
  mat beta_ls = solve(R1, Y1);
  
  
  
  // Function returns the 'p+1' by '1' matrix 
  // beta_ls of regression coefficient estimates
  return(beta_ls.t());
  
}  

/* ~~~~~~~~~~~~~~~~~~~~~~~~ 
 Problem 3: PCA based on QR 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~ */


// [[Rcpp::export()]]
List myEigen_QRC(const mat A, const int numIter = 1000){
  
  /*  
  
  Perform PCA on matrix A using your QR function, myQRC.
  Input:
  A: Square matrix
  numIter: Number of iterations
   
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
   
  */ 
  int r = A.n_rows;
  
  List output, output1, result;
  
  mat V = randn(r, r);

  for(int i=0; i <numIter - 1; i++){
    output = myQRC(V);
    mat Q = output["Q"];
    V = A * Q;
  }

  output1 = myQRC(V);
  mat R = output1["R"];
  mat Q = output1["Q"];
  mat D = diagvec(R);
  
  
  
  // Function should output a list with D and V
  // D is a vector of eigenvalues of A
  // V is the matrix of eigenvectors of A (in the 
  // same order as the eigenvalues in D.)
  result["D"] = D;
  result["V"] = Q;
  return(result);

}
  