#' plot S3 method
#' @description S3 method for objects of class linreg providing two plots of the residuals and the standardized residuals among the fitted values
#' @param x An object of class linreg
#' @param ... other arguments
#' @return List containing the two plots.
#' @name plot.linreg
#' @export

plot.linreg <- function(x, ...){
  # What data do we need?
  data = data.frame("fitted_values" = x$fitted_values, "residuals" = x$residuals, "sqr_abs_st_residuals" = sqrt(abs((x$residuals - mean(x$residuals))/ sqrt(x$residual_variance))))
  # calculate positions of absolute largest 3 residuals
  largest_res <- order(abs(data$residuals), decreasing = TRUE)[1:3]
  # calculate positions of absolute largest 3 residuals
  largest_st_res <- order(abs(data$sqr_abs_st_residuals), decreasing = TRUE)[1:3]
  
  # make the actual plots
  p1 <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(x = fitted_values, y = residuals)) +
    # How should the data points look like?
    ggplot2::geom_point(shape = 1, size = 6) +
    # Background should be white and black borders around it
    ggplot2::theme(panel.border = ggplot2::element_rect(linetype = "solid", fill = NA), panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
          plot.title = ggplot2::element_text(size = 14, hjust = 0.5), axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14)) +
    # Set title
    ggplot2::ggtitle("Residuals vs Fitted") +
    # Set axis labels including the function used for the regression in a new line
    ggplot2::labs(x=paste("Fitted values\n linreg(", paste(toString(x$call[2]), ")", sep=""), sep=""), y="Residuals") +
    # Add horizontal dotted line at y = 0.
    ggplot2::geom_hline(yintercept=0, linetype="dotted", color="grey") +
    # Add red line through the medians
    ggplot2::stat_summary(fun=median, colour="red", geom="line") +
    # label the 3 largest residuals
    ggplot2::geom_text(data = data[largest_res,], mapping = ggplot2::aes(x=fitted_values, y=residuals, label=largest_res), size=4, check_overlap = TRUE, nudge_x = -0.24)

  p2 <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(x = fitted_values, y = sqr_abs_st_residuals)) +
    # How should the data points look like?
    ggplot2::geom_point(shape = 1, size = 6) +
    # Background should be white and black borders around it
    ggplot2::theme(panel.border = ggplot2::element_rect(linetype = "solid", fill = NA), panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
          plot.title = ggplot2::element_text(size = 14, hjust = 0.5), axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14)) +
    # Set title
    ggplot2::ggtitle("Scale-Location") +
    # Set axis labels including the function used for the regression in a new line
    ggplot2::labs(x=paste("Fitted values\n linreg(", paste(toString(x$call[2]), ")", sep=""), sep=""), y=expression(sqrt(abs("Standardized Residuals")))) +
    # Add red line through the medians
    ggplot2::stat_summary(fun=mean, colour="red", geom="line") +
    # label the 3 largest residuals
    ggplot2::geom_text(data = data[largest_st_res,], mapping = ggplot2::aes(x=fitted_values, y=sqr_abs_st_residuals, label=largest_st_res), size=4, check_overlap = TRUE, nudge_x = -0.24)
  return(list(p1, p2))
}
