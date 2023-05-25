#' Plot Member attendance by Month
#'
#' @param end_date The end date for the plot
#' @param start_date The start date of the plot
#' @importFrom lubridate rollback
#' @importFrom lubridate ymd
#' @importFrom lubridate as_date
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return a line plot of attendance from the start date to the end date
#' @export
#'
#' @examples
#' plot_member_attendance_by_month()
#' plot_member_attendance_by_month(lubridate::as_date("2020-01-01"))
plot_member_attendance_by_month <- function(start_date = as_date("2008-01-01"), end_date = Sys.Date()) {
  YearMonth <- c2_groups <- da_date_of_attendance <- n <- NULL

  end_date <- lubridate::rollback(end_date)

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Member", c2_groups))

  attendance <- get_attendance() %>%
    dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
    dplyr::mutate(da_date_of_attendance = lubridate::as_date(da_date_of_attendance)) %>%
    dplyr::filter(da_date_of_attendance <= end_date & da_date_of_attendance > start_date) %>%
    dplyr::mutate(YearMonth = lubridate::rollback(da_date_of_attendance, roll_to_first = T)) %>%
    dplyr::group_by(YearMonth) %>%
    dplyr::tally()

  goc_plot(
    ggplot2::ggplot(attendance, aes(x = YearMonth, y = n)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(color = "darkgrey", xintercept = seq(start_date, end_date, by = "year")) +
      ggplot2::ggtitle("Member Attendance by Month", subtitle = paste0("From ", start_date, " to ", end_date)) +
      ggplot2::ylab("Member Attendance") +
      ggplot2::scale_x_date(name = "Year", date_breaks = "year", date_labels = "%Y")
  )
}
