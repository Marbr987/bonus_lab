#' pred S3 method
#' @description S3 method for objects of class ridgereg providing the fitted values of the linear regression
#' @param object An object of class ridgereg
#' @return vector containing fitted values
#' @name pred.ridgereg
#' @export
pred.ridgereg <- function(object){
  return(object$fitted_values)
}
