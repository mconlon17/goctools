#' Plot Supports by Staff and Type
#'
#' @param date The date on which the supports should be plotted
#'
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom lubridate as_date
#'
#' @return a plot showing the Staff members and for each Staff member, the supports they authored as a stacked bar chart
#' @export
#'
#' @examples
#' plot_supports_by_staff_and_type("2023-03-09")
plot_supports_by_staff_and_type <- function(date = Sys.Date()) {
  flo_support_date <- Staff <- last_name <- first_name <- `Support Type` <- value <- NULL

  supports <- get_supports() %>%
    dplyr::filter(flo_support_date == date) %>%
    dplyr::inner_join(get_contacts(), by = c("sa_contacts_2_id" = "id")) %>%
    dplyr::inner_join(dict_support_types(), by = c("flo_support_type" = "key")) %>%
    dplyr::mutate(Staff = paste0(last_name, ",\n", first_name)) %>%
    dplyr::rename(`Support Type` = value)

  goc_plot(
    ggplot(supports, ggplot2::aes(x = Staff, fill = `Support Type`)) +
      ggplot2::geom_bar() +
      ggplot2::ggtitle("Supports by Staff and Type",
        subtitle = format(lubridate::as_date(date), "%B %d, %Y")
      ) +
      ggplot2::ylab("Supports") +
      ggplot2::xlab("Staff")
  )
}
