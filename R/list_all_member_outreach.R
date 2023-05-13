#' All Member Outreach List
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' 
#' @return a flextable showing all members with phone and email who haven't been seen in more than a day
#' @export
#'
#' @examples
#' list_all_member_outreach()

list_all_member_outreach <- function() {
    
    c2_groups=Name=Since=da_date_of_attendance=MostRecent=`Most Recent`=id=last_name=first_name=Phone=c2_primary_phone=Email=email_address=NULL
    
    contacts <- get_contacts() %>%
        dplyr::filter(grepl("Member", c2_groups)) %>%
        dplyr::filter(!grepl("Deceased", c2_groups)) %>%
        dplyr::mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
        dplyr::inner_join(get_attendance(), by = c("id" = "contact_2_id")) %>%
        dplyr::mutate(Since = Sys.Date() - as_date(da_date_of_attendance)) %>%
        dplyr::group_by(id, Name) %>%
        dplyr::summarise(MostRecent = min(Since), c2_primary_phone = min(c2_primary_phone), email_address = min(email_address)) %>%
        dplyr::filter(MostRecent > 1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(MostRecent, Name) %>%
        dplyr::select(Name,  `Most Recent` = MostRecent, Phone = c2_primary_phone, Email = email_address)
    
    ft <- goc_table(contacts, paste("All Member Outreach for", Sys.Date()))
    ft
}
