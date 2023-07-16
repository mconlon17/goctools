#' List Staff Time
#'
#' @param name Name of an employee in the form last, first
#' @param start_date start date of the period to be reported
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom lubridate as_date
#' @importFrom lubridate %within%
#' @importFrom lubridate %--%
#'
#' @return a flextable showing name, date, time in and time out by rows
#' @export
#'
#' @examples
#' list_staff_time(name = "Reynolds, Keisha", start_date = "2022-01-01") # Fictional name
list_staff_time <- function(name, start_date) {
  Day <- Name <- `Time In` <- `Time Out` <- c2_groups <- da_date_of_attendance <- da_time_in <- NULL
  da_time_out <- first_name <- id <- last_name <- NULL

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(start_date) + 13

  ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Staff", c2_groups)) %>%
    dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
    dplyr::select(id, Name, c2_groups)

  attendance <- get_attendance(days = ndays) %>%
    dplyr::filter(lubridate::as_date(da_date_of_attendance) %within% (start_date %--% end_date))

  tab <- contacts %>%
    dplyr::left_join(attendance, by = c("id" = "contact_2_id")) %>%
    dplyr::select(Name, Day = da_date_of_attendance, `Time In` = da_time_in, `Time Out` = da_time_out) %>%
    dplyr::arrange(Name, Day, `Time In`, `Time Out`)

  ft <- goc_table(tab %>% dplyr::filter(Name == name), paste0("Attendance for ", name),
    caption = paste0("from ", start_date, " to ", end_date)
  )
  ft
}
