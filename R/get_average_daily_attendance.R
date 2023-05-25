#' Get average daily attendance
#'
#' Average daily attendance is the average of daily attendance for non-weekend days in a period of time.
#'
#' @param days The number of days from today to compute average daily attendance. days=7 is the past week.
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr top_n
#' @importFrom lubridate as_date
#' @importFrom lubridate wday
#' @return a number -- the average daily attendance for the number of days specified
#' @export
#'
#' @examples
#' get_average_daily_attendance() # last 30 days
#' get_average_daily_attendance(180) # previous half year
get_average_daily_attendance <- function(days = 30) {
  c2_groups <- date_of_attendance <- da_date_of_attendance <- NULL

  attendance <- get_attendance(days = days * 1.5) %>%
    dplyr::mutate(date_of_attendance = lubridate::as_datetime(da_date_of_attendance))

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Member", c2_groups))

  attendance <- attendance %>%
    dplyr::inner_join(contacts, by = c("contact_2_id" = "id"))

  attendance <- attendance %>%
    dplyr::group_by(date_of_attendance) %>%
    dplyr::tally() %>%
    dplyr::filter(lubridate::wday(date_of_attendance) > 1 & lubridate::wday(date_of_attendance) < 7) %>%
    dplyr::arrange(dplyr::desc(date_of_attendance)) %>%
    dplyr::top_n(days, date_of_attendance)

  ada <- mean(attendance$n)
  ada
}
