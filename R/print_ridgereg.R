#' print S3 method
#' @description S3 method for objects of class ridgereg printing the call and the coefficients of the linear model
#' @param x An object of class ridgereg
#' @param ... other arguments
#' @return nothing. Prints tha call and the coefficients of the ridgereg object.
#' @name print.ridgereg
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$regression_coefficient)
}
