#' Check Supports
#'
#' @param check The name of a check to be performed.  One of staff, member, date, duration, type, durations
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#'
#' @return a flextable with the requested checks
#' @export
#'
#' @examples
#' check_supports()
#' check_supports("duration") # list supports with no duration recorded
#' check_supports("durations") # drequency table of durations
check_supports <- function(check = "staff") {
    
    id=last_name=first_name=`Support ID`=name=Staff=Member=Date=flo_support_date=Type=flo_support_type=Note=flo_support_note=Duration=flo_support_duration=NULL
    
    rel <- get_table("flo_supports_contacts_c")
    
    members <- get_members()[, c("id", "first_name", "last_name")] %>%
        dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
        dplyr::select(id, Member)
    
    supports <- get_supports() %>%
        dplyr::left_join(get_contacts()[, c("id", "first_name", "last_name")], by = c("sa_contacts_2_id" = "id")) %>%
        dplyr::mutate(Staff = paste0(last_name, ", ", first_name)) %>%
        dplyr::left_join(rel, by = c("id" = "flo_supports_contactsflo_supports_ida")) %>%
        dplyr::left_join(members, by = c("flo_supports_contactscontacts_idb" = "id")) %>%
        dplyr::select(`Support ID`=name, Staff, Member, Date=flo_support_date, Type=flo_support_type, Note=flo_support_note, Duration=flo_support_duration)
    
    if(check=="staff") {
        tab <- supports %>%
            dplyr::filter(Staff == "NA, NA") %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Supports with no Staff Member Assigned"))
        ft
    }
    
    if (check=="member") {
        tab <- supports %>%
            dplyr::filter(is.na(Member)) %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Supports with no Member Assigned"))
        ft
    }
    
    if(check=="date"){
        tab <- supports %>%
            dplyr::filter(is.na(Date) | Date=="")
        
        ft <- goc_table(tab, paste0(nrow(tab), " Supports with no support date"))
        ft
    }
    
    if(check=="duration") {
        tab <- supports %>%
            dplyr::filter(Duration == 0 | is.na(Duration)) %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Supports with no duration"))
        ft
    }
    
    if(check=="type") {
        tab <- supports %>%
            dplyr::group_by(Type) %>%
            dplyr::tally()
        
        ft <- goc_table(tab, "Support Type")
        ft
    }
    
    if(check=="durations") {
        tab <- supports %>%
            dplyr::group_by(Duration) %>%
            dplyr::tally()
        
        ft <- goc_table(tab, "Support Durations")
        ft
    }
    
    ft
}
