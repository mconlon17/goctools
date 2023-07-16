#' Plot gifts by calendar year
#'
#' @importFrom lubridate floor_date
#' @importFrom lubridate days
#' @importFrom lubridate as_date
#' @importFrom lubridate "%within%"
#' @importFrom lubridate "%--%"
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 rel

#' @return a plot showing total raised each year in vertical bars
#' @export
#'
#' @examples
#' plot_gifts_by_year()
plot_gifts_by_year <- function() {
  Year <- `Yearly Total` <- flo_gift_amount <- flo_gift_date <- n <- NULL

  start_date <- lubridate::as_date("2007-01-01")
  end_date <- lubridate::floor_date(Sys.Date(), "year") - lubridate::days(1)

  gifts <- get_gifts() %>%
    dplyr::filter(lubridate::as_date(flo_gift_date) %within% (start_date %--% end_date)) %>%
    dplyr::mutate(Year = year(lubridate::as_date(flo_gift_date))) %>%
    dplyr::group_by(Year) %>%
    dplyr::tally()

  goc_plot(
    ggplot2::ggplot(gifts, ggplot2::aes(Year, n)) +
      ggplot2::geom_bar(fill = "blue", stat = "identity") +
      ggplot2::ggtitle("Gifts by Calendar Year", subtitle = paste0("from ", start_date, " to ", end_date)) +
      ggplot2::scale_y_continuous(name = "Gifts") +
      ggplot2::scale_x_continuous(name = "Calendar Year", breaks = seq(min(gifts$Year), max(gifts$Year)))
  )
}
