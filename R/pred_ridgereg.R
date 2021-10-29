#' pred S3 method
#' @description S3 method for objects of class ridgereg providing the fitted values of the linear regression
#' @param object An object of class ridgereg
#' @param data A dataframe for which the values should be predicted. If not provided, the training data for the object is used.
#' @param ... other arguments
#' @return vector containing fitted values
#' @name pred.ridgereg
#' @export
pred.ridgereg <- function(object, data=NULL, ...){
  if(is.null(data)) return(object$fitted_values)
  else{
    if(!(c(object$formula[[2]]) %in% colnames(data))){
      X <- model.matrix(object$formula[-2], data)
    }
    else {
      X <- model.matrix(object$formula, data)      
    }
    y_pred <- X %*% matrix(nrow=length(object$regression_coefficient), ncol=1 ,object$regression_coefficient)
    return(as.vector(y_pred))
  }
}
