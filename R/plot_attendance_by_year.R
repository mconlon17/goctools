#' Plot Attendance by Year
#'
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom dplyr tally
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return plot of attendance by year with lines for staff, members, and volunteers
#' @export
#'
#' @examples
#' plot_attendance_by_year()
plot_attendance_by_year <- function() {
  `Person Type` <- PersonType <- Year <- da_date_of_attendance <- n <- c2_groups <- NULL

  contacts <- get_contacts() %>%
    dplyr::mutate(PersonType = "Other") %>%
    dplyr::mutate(PersonType = ifelse(grepl("Staff", c2_groups), "Staff", PersonType)) %>%
    dplyr::mutate(PersonType = ifelse(grepl("Member", c2_groups), "Member", PersonType)) %>%
    dplyr::mutate(PersonType = ifelse(grepl("Volunteer", c2_groups), "Volunteer", PersonType)) %>%
    dplyr::filter(PersonType == "Member" | PersonType == "Staff" | PersonType == "Volunteer")

  attendance <- get_attendance() %>%
    dplyr::mutate(Year = lubridate::year(da_date_of_attendance)) %>%
    dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
    dplyr::rename(`Person Type` = PersonType) %>%
    dplyr::group_by(Year, `Person Type`) %>%
    dplyr::tally() %>%
    dplyr::filter(Year < year(Sys.Date()))

  goc_plot(
    ggplot2::ggplot(attendance, aes(x = Year, y = n, color = `Person Type`)) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::ggtitle("Attendance by Year and Person Type",
        subtitle = "Staff attendance recording began in 2018. Volunteer attendance recording began in 2019"
      ) +
      ggplot2::ylab("Attendance") +
      ggplot2::scale_x_continuous(name = "Year", breaks = seq(2008, lubridate::year(Sys.Date() - lubridate::years(1))))
  )
}
