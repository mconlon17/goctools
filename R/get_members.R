#' Get member data from Flourish
#'
#' @param include.deleted if TRUE, include deleted records.  Default is FALSE
#' @param active.only if TRUE, include only those members who have attended in the past 90 days, are not
#' deceased and are not on leave.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom lubridate as_date
#' 
#' @return a data.frame of member data
#' @export
#'
#' @examples
#' get_members()
#' get_members(active.only=TRUE)

get_members <- function(include.deleted = FALSE, active.only = FALSE) {
    members <- get_table("contacts", include.deleted)
    
    Since=da_date_of_attendance=flo_deceased_c=flo_membership_status_c=id=id.x=NULL
    
    if (active.only) {
        recent.members <- members %>%
            dplyr::filter(flo_deceased_c != "Yes") %>%
            dplyr::filter(flo_membership_status_c != "On_leave") %>%
            dplyr::inner_join(get_attendance(days=90), by = c("contact_id_c" = "contact_2_id")) %>%
            dplyr::mutate(Since = Sys.Date() - lubridate::as_date(da_date_of_attendance)) %>%
            dplyr::rename(id = id.x) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(MostRecent = min(Since)) %>%
            dplyr::select(id)
        members <- members %>%
            dplyr::inner_join(recent.members, by = c("id" = "id"))
    }
    members
}