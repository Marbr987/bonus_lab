#' visualize_airport_delays
#' @description A function visualizing the mean delay of flights for different airports
#' @return NA
#' @importFrom ggplot2 ggplot borders geom_point coord_quickmap theme_minimal scale_colour_gradientn labs
#' @import nycflights13
#' @importFrom dplyr group_by summarise inner_join
#' @import maps
#' @source https://jrnold.github.io/r4ds-exercise-solutions/relational-data.html
#' @export
visualize_airport_delays <- function(){
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  # Calculate mean delays (in the sense of arrival delays)
  delays <-
    flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
    dplyr::inner_join(airports, by = c("dest" = "faa"))
  delays %>%
    ggplot2::ggplot(aes(lon, lat, color=delay)) +
    ggplot2::borders("state") +
    ggplot2::geom_point() +
    ggplot2::coord_quickmap()+
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_gradientn(colors=c("darkseagreen", "royalblue1", "red2")) +
    ggplot2::labs(x="longitude", y="latitude", color="delay / min")
}
