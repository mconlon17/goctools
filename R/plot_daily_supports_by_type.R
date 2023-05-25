#' Plot Daily Supports by Type
#'
#' @param date date of plot
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return a plot showing vertical bars with counts of each support type on the day specified
#' @export
#'
#' @examples
#' plot_daily_supports_by_type() # often blank -- supports have not been entered for the current day
#' plot_daily_supports_by_type("2023-03-09")
plot_daily_supports_by_type <- function(date = Sys.Date()) {
  Type <- flo_support_date <- value <- NULL

  supports <- get_supports(with.members = T) %>%
    dplyr::filter(flo_support_date == date) %>%
    dplyr::inner_join(get_contacts(), by = c("sa_contacts_2_id" = "id")) %>%
    dplyr::inner_join(dict_support_types(), by = c("flo_support_type" = "key")) %>%
    dplyr::mutate(Type = gsub(" ", "\n", value))

  goc_plot(
    ggplot2::ggplot(supports, ggplot2::aes(Type, fill = Type)) +
      ggplot2::ggtitle("Daily Supports by Type", paste0("for ", format(lubridate::as_date(date), "%B %d, %Y"))) +
      ggplot2::ylab("Count") +
      ggplot2::xlab("Support Type") +
      ggplot2::geom_bar()
  )
}
