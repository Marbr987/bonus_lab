#' Generic pred() function for S3
#' @description Provides generic function pred() for S3 objects
#' @param object some object
#' @param ... other arguments
#' @return nothing
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}
