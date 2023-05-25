#' Check Attendance Records
#'
#' @param check One of "attendee", "date", "time_in", "time_out", "times", "location" Check looks for records with issues
#' in the corresponding column.  times checks that time_out occurs after time_in
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select

#' @return return tables of attendance records with issues
#' @export
#'
#' @examples
#' check_attendance()
check_attendance <- function(check = "times") {
  name <- da_date_of_attendance <- contact_2_id <- `Flourish ID` <- sort_order <- NULL
  Date <- Location <- da_date_of_attendance <- da_time_in <- da_time_out <- location <- `Time In` <- `Time Out` <- NULL

  attendance <- get_attendance() %>%
    dplyr::mutate(sort_order = as.numeric(substr(name, 5, length(name))))

  if (check == "attendee") {
    tab <- attendance %>%
      dplyr::filter(is.na(contact_2_id) | contact_2_id == "") %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, Date = da_date_of_attendance)


    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records without attendee"))
    ft
  }

  if (check == "date") {
    tab <- attendance %>%
      dplyr::filter(is.na(da_date_of_attendance) | da_date_of_attendance == "") %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, Date = da_date_of_attendance)

    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records without date of attendance"))
    ft
  }

  if (check == "time_in") {
    tab <- attendance %>%
      dplyr::filter(is.na(da_time_in) | da_time_in == "") %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, `Time In` = da_time_in)


    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records without Time In"))
    ft
  }

  if (check == "time_out") {
    tab <- attendance %>%
      dplyr::filter(is.na(da_time_out) | da_time_out == "") %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, `Time Out` = da_time_out)


    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records without Time Out"))
    ft
  }

  if (check == "times") {
    tab <- attendance %>%
      dplyr::filter(da_time_in > da_time_out) %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, `Time In` = da_time_in, `Time Out` = da_time_out)

    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records with Time Out before Time In"))
    ft
  }

  if (check == "location") {
    tab <- attendance %>%
      dplyr::filter(location == "Unknown" | location == "unknown" | location == "" | is.na(location)) %>%
      dplyr::arrange(desc(sort_order)) %>%
      dplyr::select(`Flourish ID` = name, Date = da_date_of_attendance, Location = location)

    ft <- goc_table(tab, paste0(nrow(tab), " Attendance records with unknown Location"))
    ft
  }
  ft
}
