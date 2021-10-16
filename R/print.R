#' print S3 method
#' @description S3 method for objects of class linreg printing the call and the coefficients of the linear model
#' @param x An object of class linreg
#' @param ... other arguments
#' @return nothing. Prints tha call and the coefficients of the linreg object.
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$regression_coefficient)
}
