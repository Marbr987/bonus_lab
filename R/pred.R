#' pred S3 method
#' @description S3 method for objects of class linreg providing the fitted values of the linear regression
#' @param object An object of class linreg
#' @param data A dataframe for which the values should be predicted. If not provided, the training data for the object is used.
#' @param ... other arguments
#' @return vector containing fitted values
#' @name pred.linreg
#' @export
pred.linreg <- function(object, data=NULL, ...){
  if(is.null(data)) return(object$fitted_values)
  else{
    X <- model.matrix(object$formula, data)
    y_pred <- X %*% object$coeff
    return(y_pred)
  }
}
