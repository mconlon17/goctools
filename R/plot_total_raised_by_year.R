#' Plot total raised by calendar year
#'
#' @param date Show gifts up to to the end of the year preceding the date
#' @importFrom lubridate floor_date
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
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
#' plot_total_raised_by_year()
plot_total_raised_by_year <- function(date=Sys.Date()) {
    
    Year=`Yearly Total`=flo_gift_amount=flo_gift_date=NULL
    
    end_date <- lubridate::floor_date(date, "year") - lubridate::days(1)
    
    gifts <- get.gifts() %>%
        dplyr::filter(flo_gift_date <= end_date) %>%
        dplyr::mutate(Year = year(as_date(flo_gift_date))) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(`Yearly Total` = sum(flo_gift_amount))
    
    ggplot2::ggplot(gifts, ggplot2::aes(Year, weight = `Yearly Total`)) +
        ggplot2::geom_bar(fill = "#79B956") +
        ggplot2::ggtitle("Total Dollars Raised by Calendar Year") +
        ggplot2::scale_y_continuous(name="Total Dollars Raised", breaks=seq(25000, 75000, by=25000), 
                           labels=c("$25,000", "$50,000", "$75,000")) +
        ggplot2::scale_x_continuous(name="Calendar Year", breaks=seq(min(gifts$Year),max(gifts$Year))) +
        ggplot2::theme_linedraw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(2.5), face = "bold")) +
        ggplot2::theme(axis.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(1.1), face = "bold")) +
        ggplot2::theme(axis.text  = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(1.2)))
}