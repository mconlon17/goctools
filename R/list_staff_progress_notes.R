#' Staff Progress Notes
#'
#' @param date A date in the interval of interest
#' @param interval One of "week", "month" year", "all"
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom lubridate "%within%"
#' @importFrom lubridate "%--%"
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate days
#'
#' @return a flex table of progress notes by staff member in the interval requested
#' @export
#'
#' @examples
#' list_staff_progress_notes()
#' list_staff_progress_notes(date="2023-04-05", interval="month") # progress notes for April

list_staff_progress_notes <- function(date = Sys.Date(), interval = "month") {
    
    start_date=end_date=`Note Date`=NULL
    Member=PNDate=Staff=c2_groups=contact_2_id=contact_id=date_modified=NULL
    first_name=flo_date_of_note=last_name=progress_note=Note=NULL
    
    
    date <- lubridate::as_date(date)
    
    if (interval == "all") {
        start_date <- lubridate::as_date(-Inf)
        end_date <- lubridate::as_date(Inf)
    } else {
        start_date <- lubridate::floor_date(date, interval)
        end_date <- lubridate::ceiling_date(date, interval) - lubridate::days(1)
    }
    
    pn <- get_progress_notes() %>%
        dplyr::mutate(flo_date_of_note = as_date(flo_date_of_note)) %>%
        dplyr::mutate(date_modified = as_date(date_modified)) %>%
        dplyr::mutate(PNDate = as_date(ifelse(date_modified > flo_date_of_note &
                                           date_modified != ymd("2023-02-21"), date_modified, flo_date_of_note))) %>%
        dplyr::select(contact_id, contact_2_id, PNDate, date_modified, flo_date_of_note, progress_note)
    
    contacts <- get_contacts() %>%
        dplyr::filter(grepl("Staff", c2_groups)) %>%
        dplyr::mutate(Staff = paste0(last_name, ", ", first_name))
    
    members <- get_members() %>%
        dplyr::mutate(Member=paste0(last_name, ", ", first_name))
    
    tab <- pn %>%
        dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
        dplyr::left_join(members, by = c("contact_id" = "id")) %>%
        dplyr::filter(PNDate %within% (start_date %--% end_date)) %>%
        dplyr::arrange(Staff, Member, PNDate) %>%
        dplyr::select(Staff, Member, `Note Date`=PNDate, Note=progress_note)
    
    ft <- goc_table(tab, "Progress Notes By Staff", caption=ifelse(is.infinite(start_date), "All time", 
                                                                   paste0("from ", start_date, " to ", end_date)))
    ft
    
}
