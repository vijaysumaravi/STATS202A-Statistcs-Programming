n = 50
p = 200
s = 10
T = 10
lambda_all = (100:1)*10
L = length(lambda_all)
db = matrix(rep(0, p), nrow = p)

X = matrix(rnorm(n*p), nrow=n)
X = cbind(rep(1,n), X)
beta_true = matrix(rep(0, p+1), nrow = p+1)
beta_true[1:s] = 1:s
Y = X %*% beta_true + rnorm(n)

beta = matrix(rep(0, p+1), nrow = p+1)
beta_all = matrix(rep(0, (p+1)*L), nrow = p+1)
err = rep(0, L)
R = Y
ss = rep(0, p+1)
for (j in 1:p+1)
  ss[j] = sum(X[, j]^2)

for (l in 1:L)
{
  lambda = lambda_all[l]
  for (t in 1:T)
  {
    for (j in 2:p+1)
    {
      db = sum(R*X[, j])/ss[j]
      b = beta[j]+db
      b=sign(b)*max(0,abs(b)-lambda/ss[j])
      db=b-beta[j]
      R=R-X[,j]*db
      beta[j]=b
    }
  }
  beta_all[, l] = beta
  err[l] = sum((beta-beta_true)^2)
}

plot(lambda_all, err, type = 'l',main='LASSO ERROR ESTIMATION',xlab='Lambda',ylab='Error')
