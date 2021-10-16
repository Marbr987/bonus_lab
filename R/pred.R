#' pred S3 method
#' @description S3 method for objects of class linreg providing the fitted values of the linear regression
#' @param object An object of class linreg
#' @return vector containing fitted values
#' @name pred.linreg
#' @export
pred.linreg <- function(object){
  return(object$fitted_values)
}
