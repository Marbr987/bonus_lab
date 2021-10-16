#' coef S3 method
#' @description S3 method for objects of class linreg providing the coefficients of the linear regression
#' @param object An object of class linreg
#' @param ... other arguments
#' @return coefficients of linear regression
#' @name coef.linreg
#' @export
coef.linreg <- function(object, ...){
  return(object$regression_coefficient)
}
