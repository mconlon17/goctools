#' List Birthdays
#'
#' Lists member birthdays
#'
#' @param date The month to list the birthdays specified by any day within the month.
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom lubridate %--%
#' @importFrom lubridate %within%
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ymd
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#'
#' @return a flextable listing the members with birthdays during the month, ordered by birthday
#' @export
#'
#' @examples
#' list_birthdays()
list_birthdays <- function(date = Sys.Date()) {
  `First Name` <- `Last Name` <- Birthday <- birthday <- birthdate <- first_name <- last_name <- NULL

  start_date <- lubridate::floor_date(date, "month")
  end_date <- lubridate::ceiling_date(date, "month") - days(1)

  members <- get_members(active.only = T) %>%
    dplyr::mutate(birthday = lubridate::as_date(ifelse(birthdate == "", NA,
      lubridate::ymd(paste(lubridate::year(Sys.Date()),
        lubridate::month(birthdate), lubridate::day(birthdate),
        sep = "-"
      ))
    ))) %>%
    dplyr::filter(birthday %within% (start_date %--% end_date)) %>%
    dplyr::arrange(birthday, last_name, first_name) %>%
    dplyr::select(`First Name` = first_name, `Last Name` = last_name, Birthday = birthday)

  ft <- goc_table(members, paste0("Active Member Birthdays for ", format(date, "%B")))
  ft
}
