#' List Monthly Progress Notes
#'
#' @param date date in month for desired report
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom lubridate "%within%"
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate days
#'
#' @return a flextable of members who attended during the month, with their progress notes
#' @export
#'
#' @examples
#' list_monthly_progress_notes()
#' list_monthly_progress_notes("2203-03-05")

list_monthly_progress_notes <- function(date = Sys.Date()) {
    
    `Date of Note`=Member=Name=c2_groups=contact_2_id=contact_id_c=NULL
    created_by=da_date_of_attendance=date_entered=date_modified=NULL
    date_modified.y=first_name=id=last_name=modified_user_id=NULL
    progress_note=NULL
    
    date <- lubridate::as_date(date)
    start_date <- lubridate::floor_date(date, "month")
    end_date <- lubridate::ceiling_date(date, "month") - lubridate::days(1)
    
    ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1
    
    contacts <- get_contacts() %>%
        dplyr::filter(grepl("Member", c2_groups)) %>%
        dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
        dplyr::select(id, Member)
    
    contacts <- get_attendance(days = ndays) %>%
        dplyr::filter(lubridate::as_date(da_date_of_attendance) %within% (start_date %--% end_date)) %>%
        dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
        dplyr::group_by(contact_2_id, Member) %>%
        dplyr::tally() %>%
        dplyr::rename(`Times\nAttended` = n)
    
    pn <- get_progress_notes() %>%
        dplyr::filter(as_date(date_entered) %within% (start_date %--% end_date) |
                   as_date(date_modified) %within% (start_date %--% end_date)) %>%
        dplyr::filter(modified_user_id != "4cde6014-5735-b6a8-2975-63e1844134be") %>%
        dplyr::filter(created_by != "4cde6014-5735-b6a8-2975-63e1844134be")

    pn <- get_members(active.only = T) %>%
        dplyr::mutate(Name = paste0(last_name, ", ", first_name)) %>%
        dplyr::inner_join(pn, by = c("id" = "contact_id")) %>% # May have a progress_note, left_join
        dplyr::select(contact_id_c, Name, date_modified.y, progress_note)


    tab <- contacts %>%
        dplyr::left_join(pn, by = c("contact_2_id" = "contact_id_c")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date_modified.y = as_date(date_modified.y)) %>%
        dplyr::rename(`Date of Note` = date_modified.y, `Progress Note` = progress_note) %>%
        dplyr::select(-contact_2_id, -Name) %>%
        dplyr::arrange(Member, `Date of Note`)

    ft <- goc_table(tab, paste0("Members who attended with attendance count and progress notes for ", date))
    ft
}
