#' Plot Members By Year
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_line
#'
#' @return a plot of members by year
#' @export
#'
#' @examples
#' plot_members_by_year()
plot_members_by_year <- function() {
  Year <- `Number of Members` <- NULL

  goc_plot(
    goc_plot(ggplot2::ggplot(list_members_by_year(output = "data.frame"), ggplot2::aes(Year, `Number of Members`)) +
      ggplot2::ggtitle("Members by Year", subtitle = " ") +
      ggplot2::geom_line())
  )
}
