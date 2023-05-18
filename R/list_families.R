#' List Family Contact Info
#'
#' @return a flextable with all families and their contact information
#' @export
#'
#' @examples
#' list_families()
list_families <- function() {
    
    `Name` = contact2_full_name = `Phone` = c2_primary_phone = last_name = first_name = NULL
    `Street Address` = primary_address_street = `City` = primary_address_city = `State` = primary_address_state = NULL
    `Zip` = primary_address_postalcode = `Email` = email_address = c2_contact_record_type_c = NULL
    
    tab <- get_contacts() %>%
        
        dplyr::filter(c2_contact_record_type_c == "Family") %>%
        arrange(last_name, first_name) %>%
        dplyr::select( `Name` = contact2_full_name, `Phone` = c2_primary_phone,
                `Street Address` = primary_address_street, `City` = primary_address_city, `State` = primary_address_state,
                `Zip` = primary_address_postalcode, `Email` = email_address)

        ft <- goc_table(tab, "Family Contact Info", paste0(nrow(tab), " families"))
        ft
}
