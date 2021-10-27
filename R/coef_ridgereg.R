#' coef S3 method
#' @description S3 method for objects of class ridgereg providing the coefficients of the linear regression
#' @param object An object of class ridgereg
#' @param ... other arguments
#' @return coefficients of linear regression
#' @name coef.ridgereg
#' @export
coef.ridgereg <- function(object, ...){
  return(object$regression_coefficient)
}
