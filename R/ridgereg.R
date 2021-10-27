#' ridgereg
#' @description A function that performs a ridge regression based on QR decomposition
#' @param formula A formula stating the form of the regression
#' @param data A dataframe to use for the regression
#' @param lambda A numeric representing the lambda value for the ridge regression
#' @return An object of class linreg containing regression coefficients, fitted values, residuals, degree-of-freedom, residual variance, variance of the regression coefficient, t-values and p-values
#' @source http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf
#' @source http://pages.stat.wisc.edu/~larget/math496/qr.html
#' @source https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
#' @importFrom stats median model.matrix pt residuals sd
#' @export
ridgereg <- function(formula, data, lambda){
  call = match.call()
  X <- model.matrix(formula, data)
  sd_X <- c(1)
  mean_X <- c(1)
  X_stand <- matrix(nrow=nrow(X), ncol=ncol(X))
  X_stand[,1] <- X[,1]
  for (i in 2:ncol(X)) {
    sd_X[i] <- sd(X[,i])
    mean_X[i] <- mean(X[,i])
    if(sd_X[i] != 0){
      X_stand[,i] <- (X[,i] - mean_X[i]) / sd_X[i]      
    }
  }
  y_var_name <- all.vars(formula)[1]
  y <- data[[y_var_name]]
  y_org <- y
  mean_y <- mean(y)
  y <- y - mean_y
  y <- c(y, rep(0, ncol(X_stand)))
  Y <- rbind(X_stand, sqrt(lambda) * diag(ncol(X_stand)))
  QR <- qr(Y)
  Q <- qr.Q(QR)
  R <- qr.R(QR)
  coeff_stand <- qr.coef(QR, y)
  coeff <- coeff_stand / sd_X
  coeff[1] <- coeff[1] - as.numeric(coeff[-1] %*% mean_X[-1]) + mean_y
  names(coeff) <- colnames(X)
  y_pred <- X %*% coeff
  y_pred <- as.numeric(y_pred[,1])
  res <- list(call = call,
              regression_coefficient = coeff,
              fitted_values = y_pred
              )
  class(res) <- "ridgereg"
  return(res)
}


