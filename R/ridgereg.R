#' ridgereg
#' @description A function that performs a ridge regression based on QR decomposition
#' @param formula A formula stating the form of the regression
#' @param data A dataframe to use for the regression
#' @return An object of class linreg containing regression coefficients, fitted values, residuals, degree-of-freedom, residual variance, variance of the regression coefficient, t-values and p-values
#' @source http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf
#' @source http://pages.stat.wisc.edu/~larget/math496/qr.html
#' @source https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
#' @export
ridgereg <- function(formula, data, lambda){
  call = match.call()
  X <- model.matrix(formula, data)
  y_var_name <- all.vars(formula)[1]
  y <- data[[y_var_name]]
  y <- c(y, rep(0, ncol(X)))
  Y <- rbind(X, sqrt(lambda) * diag(ncol(X)))
  QR <- qr(Y)
  Q <- qr.Q(QR)
  R <- qr.R(QR)
  coeff <- qr.coef(QR, y)
  residuals <- qr.resid(QR, y)
  y_pred <- X %*% coeff
  y_pred <- as.numeric(y_pred[,1])
  df <- nrow(X) - ncol(X)
  residual_variance <- t(residuals) %*% residuals / df
  residual_variance <- residual_variance[1,1]
  var_coeff <- residual_variance * solve(t(R) %*% R)
  t_values <- coeff / sqrt(diag(var_coeff))
  p_values <- sapply(2*(1 - pt(abs(t_values), df)), function(x) {if(x < 2e-16) {"<2e-16"} else round(x,5)})
  res <- list(call = call,
              regression_coefficient = coeff,
              fitted_values = y_pred,
              residuals = residuals,
              degree_of_freedom = df,
              residual_variance = residual_variance,
              variance_of_regression_coefficient = var_coeff,
              t_values = t_values,
              p_values = p_values)
  class(res) <- "ridgereg"
  return(res)
}
