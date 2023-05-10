#' Plot gifts by calendar year
#'
#' @param date Show gifts up to to the end of the year preceding the date
#' @importFrom lubridate floor_date
#' @importFrom lubridate days
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
plot_gifts_by_year <- function(date=Sys.Date()) {
    
    Year=`Yearly Total`=flo_gift_amount=flo_gift_date=n=NULL
    
    end_date <- lubridate::floor_date(date, "year") - lubridate::days(1)
    
    gifts <- get_gifts() %>%
        dplyr::filter(flo_gift_date <= end_date) %>%
        dplyr::mutate(Year = year(as_date(flo_gift_date))) %>%
        dplyr::group_by(Year) %>%
        dplyr::tally()
    
    ggplot2::ggplot(gifts, ggplot2::aes(Year, n)) +
        ggplot2::geom_bar(fill = "blue", stat="identity") +
        ggplot2::ggtitle("Gifts by Calendar Year") +
        ggplot2::scale_y_continuous(name = "Gifts") +
        ggplot2::scale_x_continuous(name = "Calendar Year", breaks = seq(min(gifts$Year),max(gifts$Year))) +
        ggplot2::theme_linedraw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(2.5), face = "bold")) +
        ggplot2::theme(axis.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(1.1), face = "bold")) +
        ggplot2::theme(axis.text  = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(1.2)))
}