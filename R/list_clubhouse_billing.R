#' Clubhouse Billing List
#'
#' Create a flextable that shows the billing line items needed for Lutheran Services (LSF) KIS (Knight
#' Information Systems) billing. The report processes attendance data for staff and members and
#' creates billable time records for each staff member.
#'
#' @param date The date on which the billing list will end.  The start date is always the first of the
#' month of the month that contains the end date.  So, for example, if the date is set to 2023-05-18,
#' the start date is 2023-05-01 and the end date is 2023-05-18.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom lubridate floor_date
#' @importFrom lubridate as_date
#' @importFrom lubridate as_datetime
#' @importFrom lubridate %within%
#' @importFrom lubridate dhours
#' @importFrom lubridate dminutes
#'
#' @return a flextable of billing line items suitable for entry into the KIS system required by LSF.
#' @export
#'
#' @examples
#' list_clubhouse_billing()
#' list_clubhouse_billing("2023-05-13") # Through the first two weeks of the month
list_clubhouse_billing <- function(date = Sys.Date()) {
  start_date <- BillInterval <- Date <- End <- Hours <- MembersPresent <- Minutes <- Name <- StaffPresent <- NULL
  Start <- `Total Minutes` <- c2_groups <- contact_2_id <- da_date_of_attendance <- NULL
  da_time_in <- da_time_out <- dhours <- dminutes <- first_name <- id <- last_name <- TotalMinutes <- NULL

  start_date <- lubridate::floor_date(lubridate::as_date(date), "month")
  end_date <- lubridate::as_date(date)

  ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

  members <- get_contacts() %>%
    dplyr::filter(grepl("Member", c2_groups)) %>%
    dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
    dplyr::select(id, Name)

  staff <- get_contacts() %>%
    dplyr::filter(grepl("Staff", c2_groups)) %>%
    dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
    dplyr::select(id, Name)

  member.attendance <- get_attendance(days = ndays) %>%
    dplyr::filter(lubridate::as_date(da_date_of_attendance) %within% (start_date %--% end_date)) %>%
    dplyr::inner_join(members, by = c("contact_2_id" = "id")) %>%
    dplyr::select(Name, contact_2_id, da_date_of_attendance, da_time_in, da_time_out) %>%
    dplyr::arrange(da_date_of_attendance)

  staff.attendance <- get_attendance(days = ndays) %>%
    dplyr::filter(lubridate::as_date(da_date_of_attendance) %within% (start_date %--% end_date)) %>%
    dplyr::inner_join(staff, by = c("contact_2_id" = "id")) %>%
    dplyr::mutate(StaffPresent = lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_in)) %--%
      lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_out))) %>%
    dplyr::select(Name, contact_2_id, da_date_of_attendance, StaffPresent, da_time_in, da_time_out) %>%
    dplyr::arrange(Name, StaffPresent)

  member.intervals <- member.attendance %>%
    dplyr::group_by(da_date_of_attendance) %>%
    dplyr::summarise(MembersPresent = min(lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_in)), na.rm = T) %--%
      max(lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_out)), na.rm = T))

  bill.intervals <- staff.attendance %>%
    dplyr::inner_join(member.intervals, by = "da_date_of_attendance") %>%
    dplyr::mutate(BillInterval = make_intersection_v(StaffPresent, MembersPresent)) %>%
    dplyr::mutate(Hours = floor(BillInterval / lubridate::dhours(1))) %>%
    dplyr::mutate(Minutes = BillInterval / lubridate::dminutes(1) - 60 * Hours) %>%
    dplyr::mutate(Start = format(lubridate::int_start(BillInterval), "%I:%M %p")) %>%
    dplyr::mutate(End = format(lubridate::int_end(BillInterval), "%I:%M %p")) %>%
    dplyr::mutate(TotalMinutes = 60 * Hours + Minutes) %>%
    dplyr::select(Name, Date = da_date_of_attendance, Start, End, `Total Minutes` = TotalMinutes) %>%
    dplyr::arrange(Date, Name)

  ft <- goc_table(bill.intervals, paste0("From ", start_date, " to ", end_date))
  ft
}
