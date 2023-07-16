#' Plot Membership by the Numbers
#'
#' @param date Date in month to end the display
#' @param months past months to include in plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 scale_x_date
#' @importFrom patchwork plot_annotation
#' @importFrom patchwork plot_layout
#'
#' @return a 12 panel plot showing various statistics over months requested: unique members, average workday hours, average daily attendance, new members, supports, members with supports, pre-employment supports, transitional employment supports, supported employment supports, independent employment supports, donors, amount raised
#' @export
#'
#' @examples
#' plot_membership_by_the_numbers()
plot_membership_by_the_numbers <- function(date = Sys.Date(), months = 15) {
  `Amount Raised` <- `Average Daily Attendance` <- `Average Workday Hours` <- Donors <- NULL
  `IE Supports` <- `Members with Supports` <- Month <- `New Members` <- `Pre-employment Supports` <- `SE Supports` <- NULL
  Supports <- `TE Supports` <- `Unique Members` <- NULL

  goc_mini_plot <- function(p) {
    p + ggplot2::theme_linedraw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::scale_x_date(date_breaks = "3 month", date_labels = "%m-%y") +
      ggplot2::theme_linedraw()
  }

  start_date <- lubridate::ceiling_date(lubridate::as_date(date) - lubridate::dmonths(months + 1), "month")
  end_date <- lubridate::floor_date(lubridate::as_date(date), "month") - lubridate::ddays(1)

  data <- list_membership_by_the_numbers(date, months = months, output = "data.frame")

  p1 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Unique Members`)) +
    geom_line())
  p2 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Average Workday Hours`)) +
    geom_line())
  p3 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Average Daily Attendance`)) +
    geom_line())
  p4 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `New Members`)) +
    ggplot2::geom_col())
  p5 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Supports`)) +
    geom_line())
  p6 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Members with Supports`)) +
    geom_line())
  p7 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Pre-employment Supports`)) +
    ggplot2::geom_col())
  p8 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `TE Supports`)) +
    ggplot2::geom_col())
  p9 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `SE Supports`)) +
    ggplot2::geom_col())
  p10 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `IE Supports`)) +
    ggplot2::geom_col())
  p11 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Donors`)) +
    geom_line())
  p12 <- goc_mini_plot(ggplot(data, aes(x = Month, y = `Amount Raised`)) +
    geom_line())

  ((p1 | p2 | p3 | p4) /
    (p5 | p6 | p7 | p8) /
    (p9 | p10 | p11 | p12)) + patchwork::plot_annotation(title = paste0(
    "GOC monthly data from ",
    start_date, " to ", end_date
  ))
}
