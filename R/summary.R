#' summary S3 method
#' @description S3 method for objects of class linreg providing a summary of the object
#' @param object An object of class linreg
#' @param ... other arguments
#' @return nothing. Prints a summary of the linreg object.
#' @name summary.linreg
#' @export
summary.linreg <- function(object, ...){
  add_stars <- function(x){
    if(x != "<2e-16"){x <- as.numeric(x)}
    if(x < 0.001 | x == "<2e-16"){'***'}
    else if (x < 0.01) {'**'}
    else if (x < 0.05) {'*'}
    else if (x < 0.1) {'.'}
    else {'not defined'}
  }
  coeff_df <- data.frame("Estimate" = object$regression_coefficient, "Std. Error" = sqrt(diag(object$variance_of_regression_coefficient)), "t value" = object$t_values, "p value" = object$p_values)
  coeff_df[,5] <- sapply(coeff_df[,4], add_stars)
  cat('Coefficients:\n')
  print(coeff_df)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\nResidual standard error:", sqrt(object$residual_variance), 'on', object$degree_of_freedom, 'degrees of freedom')
}
