#' Attendance Checklist
#' 
#' For a given date, show attendance for that date -- name, time in, time out, and hours. Include
#' staff whether there is an attendance record or not.
#' 
#' @param date Date for the attendance checklist
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom lubridate as_date
#' @importFrom lubridate as_datetime
#'
#' @return a flextable with four columns -- name, time in, time out, and hours.  Blank indicates no value.
#' @export
#'
#' @examples
#' list_attendance_checklist()
#' list_attendance_checklist("2023-05-04")

list_attendance_checklist <- function(date=Sys.Date()) {
    
    da_date_of_attendance=Hours=Name=`Time In`=`Time Out`=da_time_in=da_time_out=first_name=last_name=NULL
    
    contacts <- get_contacts()
    
    ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(date)) + 1
    
    tab <- get_attendance(days = ndays) %>%
        dplyr::filter(da_date_of_attendance == date) %>%
        dplyr::left_join(contacts, by = c("contact_2_id" = "id")) %>%
        dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
        dplyr::mutate(da_time_in =lubridate::as_datetime(ifelse(is.na(da_time_in),  NA, 
                                              lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_in))))) %>%
        dplyr::mutate(da_time_out=lubridate::as_datetime(ifelse(is.na(da_time_out), NA, 
                                              lubridate::as_datetime(paste0(da_date_of_attendance, " ", da_time_out))))) %>%
        dplyr::mutate(`Time In` = format(da_time_in, "%I:%M %p")) %>%
        dplyr::mutate(`Time Out` = format(da_time_out, "%I:%M %p")) %>%
        dplyr::mutate(Hours = ifelse(is.na(da_time_out) | is.na(da_time_in) ," ",
                              format(round(as.numeric(difftime(as.POSIXct(da_time_out), 
                                                               as.POSIXct(da_time_in), units = "hours")), 2), nsmall = 2))) %>%
        dplyr::select(Name, `Time In`, `Time Out`, Hours) %>%
        dplyr::full_join(data.frame(Name = get_staff_names()), by="Name") %>%
        dplyr::arrange(Name)
    
    
    ft <- goc_table(tab, paste0("Checklist for ", format(as_date(date), "%B %d, %Y")))
    ft
}
