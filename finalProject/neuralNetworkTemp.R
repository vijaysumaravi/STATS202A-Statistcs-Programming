my_NN <- function(X_train, Y_train, X_test, Y_test, num_hidden = 20,
num_iterations = 1000, learning_rate = 1e-1)
{
	n <- dim(X_train)[1]
	p <- dim(X_train)[2] + 1
	ntest <- dim(X_test)[1]
	X_train1 <- cbind(rep(1, n), X_train)
	X_test1 <- cbind(rep(1, ntest), X_test)
	alpha <- matrix(rnorm(p * num_hidden), nrow = p)
	beta <- matrix(rnorm((num_hidden + 1)), nrow = num_hidden + 1)
	acc_train <- rep(0, num_iterations)
	acc_test <- rep(0, num_iterations)

	for(it in 1:num_iterations)
	{
		Z <- 1 / (1 + exp(-X_train1 %*% alpha))
		Z1 <- cbind(rep(1, n), Z)
		pr <- 1 / (1 + exp(-Z1 %*% beta))
		dbeta = matrix(rep(1, n), nrow = 1) %*%((matrix(Y-pr, n, m+1)*Z1))/n;
		beta <- beta + learning_rate * t(dbeta)
		for(k in 1:num_hidden)
		{
		da <- (Y_train - pr)*beta[k+1]*Z[, k]*(1-Z[, k])
		dalpha <- matrix(rep(1, n), nrow = 1)%*%((matrix(da, n, p)*X_train1))/n
		alpha[, k] <- alpha[, k] + learning_rate * t(dalpha)
	}
	
	acc_train[it] <- accuracy(pr, Y_train)
	Ztest <- 1/(1 + exp(-X_test1 %*% alpha))
	Ztest1 <- cbind(rep(1, ntest), Ztest)
	prtest <- 1/(1 + exp(-Ztest1 %*% beta))
	acc_test[it] <- accuracy(prtest, Y_test)
	cat("On iteration ", it, " the training accuracy is ", acc_train[it],
	" and the testing accuracy is ", acc_test[it], sep = "")
	}
	model <- list(alpha = alpha, beta = beta,
	acc_train = acc_train, acc_test = acc_test)
	model
}