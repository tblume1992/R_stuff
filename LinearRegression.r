OLS <- function(y, X) {
  X <- as.matrix(X)
  X_transpose <- t(X)
  XX_inv <- solve (X_transpose %*% as.matrix(X))
  beta <- XX_inv %*% X_transpose %*% as.matrix(y)
  fitted <- X %*% beta
  return(list(beta, fitted))
}

Ridge <- function(y, X, alpha) {
  X <- as.matrix(X)
  X_transpose <- t(X)
  I = diag(ncol(X)) 
  XX_inv <- solve (X_transpose %*% as.matrix(X) + alpha*I)
  beta <- XX_inv %*% X_transpose %*% as.matrix(y)
  fitted <- X %*% beta
  return(list(beta, fitted))
}
