#' Check Progress Notes
#'
#' @param check The check to be performed.  One of staff, member, goal, text
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#'
#' @return a flextable containing the requested checks
#' @export
#'
#' @examples
#' check_progress_notes()
#' check_progress_notes("staff")

check_progress_notes <- function(check = "staff") {
    
    Member=last_name=first_name=`PN ID`=name=Staff=Member=Goal=flo_goal_description=Note=progress_note=Date=flo_date_of_note=NULL
    
    pn <- get_progress_notes() %>%
        dplyr::left_join(get_members()[, c("id", "first_name", "last_name")], by = c("contact_id" = "id")) %>%
        dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
        dplyr::select(-first_name, -last_name) %>%
        dplyr::left_join(get_contacts()[, c("id", "first_name", "last_name")], by = c("contact_2_id" = "id")) %>%
        dplyr::mutate(Staff = paste0(last_name, ", ", first_name)) %>%
        dplyr::left_join(get_goals()[, c("id", "flo_goal_description", "flo_goal_type")], by = c("goal_id" = "id")) %>%
        dplyr::select(`PN ID`=name, Staff, Member, Goal=flo_goal_description, Note=progress_note, Date=flo_date_of_note)
    
    if(check=="staff") {
        tab <- pn %>%
            dplyr::filter(Staff == "NA, NA") %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Progress Notes with no Staff Member Assigned"))
        ft
    }
    
    if(check=="member") {
        tab <- pn %>%
            dplyr::filter(Member == "NA, NA") %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Progress Notes with no Member Assigned"))
        ft
    }
    
    if (check=="goal") {
        tab <- pn %>%
            dplyr::filter(is.na(Goal)) %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Progress Notes not linked to Goal"))
        ft
    }
    
    if(check=="text") {
        tab <- pn %>%
            dplyr::filter(is.na(Note) | Note == "") %>%
            dplyr::arrange(desc(Date))
        
        ft <- goc_table(tab, paste0(nrow(tab), " Empty Progress Notes"))
        ft
    }
    
    ft
}
