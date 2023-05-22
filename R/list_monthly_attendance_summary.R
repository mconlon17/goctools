#' Monthly Attendance Summary
#'
#' @param date Date to include the month to be displayed
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ddays
#'
#' @return a flextable showing for each member, the number of hours attended
#' @export
#'
#' @examples
#' list_monthly_attendance_summary()
#' list_monthly_attendance_summary("2022-03-05")

list_monthly_attendance_summary <- function(date=Sys.Date()) {
    
    Name=RowHolidayHrs=RowSocialHrs=RowWorkDayHrs=c2_groups=NULL
    da_date_of_attendance=da_total_hours=first_name=last_name=NULL
    
    date <- lubridate::as_date(date)
    start_date <- lubridate::floor_date(date, "month")
    end_date <- lubridate::ceiling_date(date, "month") - lubridate::ddays(1)
    holidays <- lubridate::as_date(get_holidays())
    ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

    
    contacts <- get_contacts() %>%
        dplyr::filter(grepl("Member", c2_groups)) %>%
        dplyr::mutate(Name = paste0(last_name, ", ", first_name))

    summary <- get_attendance(days = ndays) %>%
        dplyr::filter(as_date(da_date_of_attendance) %within% (start_date %--% end_date)) %>%
        dplyr::filter(da_total_hours < 24) %>%
        dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
        dplyr::mutate(RowWorkDayHrs = ifelse(!(da_date_of_attendance %in% holidays) &
                                          (wday(da_date_of_attendance) > 1 &
                                               wday(da_date_of_attendance) < 7), da_total_hours, 0)) %>%
        dplyr::mutate(RowSocialHrs = ifelse(!(da_date_of_attendance %in% holidays) &
                                         (wday(da_date_of_attendance) == 1 |
                                              wday(da_date_of_attendance) == 7), da_total_hours, 0)) %>%
        dplyr::mutate(RowHolidayHrs = ifelse(da_date_of_attendance %in% holidays, da_total_hours, 0)) %>%
        dplyr::group_by(Name) %>%
        dplyr::summarise(`Work Day Hours` = sum(RowWorkDayHrs), `Social Hours` = sum(RowSocialHrs), 
                  `Holiday Hours` = sum(RowHolidayHrs), `Total Hours` = sum(da_total_hours))

    summary <- dplyr::bind_rows(summary, summarise(summary,across(where(is.numeric), sum), 
                                                   across(where(is.character), ~"Total")))

    summary[which(summary[, "Name"] == "Total"), 1] <-
        paste("Totals for", nrow(summary[, "Name"]) - 1, "Members")

    ft <- goc_table(summary, paste0("Monthly Attendance Summary for ", format(start_date, "%B, %Y")))
    ft
}
