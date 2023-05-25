#' Plot total raised by calendar year
#'
#' @importFrom lubridate floor_date
#' @importFrom lubridate days
#' @importFrom lubridate "%within%"
#' @importFrom lubridate "%--%"
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
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
plot_total_raised_by_year <- function() {
    
    Year=`Yearly Total`=flo_gift_amount=flo_gift_date=start_date=NULL
    
    start_date <- lubridate::as_date("2017-01-01")
    end_date <- lubridate::floor_date(Sys.Date(), "year") - lubridate::days(1)
    
    gifts <- get_gifts() %>%
        dplyr::filter(lubridate::as_date(flo_gift_date) %within% (start_date %--% end_date)) %>%
        dplyr::mutate(Year = year(as_date(flo_gift_date))) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(`Yearly Total` = sum(flo_gift_amount))
    
    goc_plot(
        ggplot2::ggplot(gifts, ggplot2::aes(Year, weight = `Yearly Total`)) +
            ggplot2::geom_bar(fill = "green") +
            ggplot2::ggtitle("Total Dollars Raised by Calendar Year",
                             subtitle=paste0("from ", start_date, " to ", end_date)) +
            ggplot2::scale_y_continuous(name="Total Dollars Raised", breaks=seq(25000, 75000, by=25000), 
                                        labels=c("$25,000", "$50,000", "$75,000")) +
            ggplot2::scale_x_continuous(name="Calendar Year", breaks=seq(min(gifts$Year),max(gifts$Year)))
    )
}