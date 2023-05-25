#' List Last Attendnce in a Month
#'
#' @param date The month of interest, specified by any day in the month.  For example: "2023-03-20"
#' produces a list of the last attendance of members in the month March, 2023
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr desc
#' @importFrom lubridate %within%
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate as_date
#' @importFrom lubridate days
#'
#' @return a flextable listing the date of last attendance for each member in the speciefied month
#' @export
#'
#' @examples
#' list_last_attendance()
#' list_last_attendance("2023-01-03")
list_last_attendance <- function(date = Sys.Date()) {
  start_date <- end_date <- c2_groups <- Name <- last_name <- first_name <- id <- da_date_of_attendance <- `Last Visit` <- NULL

  start_date <- lubridate::floor_date(lubridate::as_date(date), "month")
  end_date <- lubridate::ceiling_date(lubridate::as_date(date), "month") - lubridate::days(1)

  ndays <- as.integer(as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Member", c2_groups)) %>%
    dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
    dplyr::select(Name, id)

  attendance <- get_attendance(days = ndays) %>%
    dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
    dplyr::filter(lubridate::as_date(da_date_of_attendance) %within% (start_date %--% end_date)) %>%
    dplyr::arrange(Name, dplyr::desc(da_date_of_attendance)) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarise(`Last Visit` = max(da_date_of_attendance)) %>%
    dplyr::select(Name, `Last Visit`)

  ft <- goc_table(attendance, paste0("Last Attendance in ", format(lubridate::as_date(date), "%B, %Y")))
  ft
}
