#' List Contacts
#'
#' @param type One of "families" "people" "organizations"
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#'
#' @return a flextable with selected contacts
#' @export
#'
#' @examples
#' list_contacts()
list_contacts <- function(type = "families") {
  Name <- contact2_full_name <- Phone <- c2_primary_phone <- last_name <- first_name <- NULL
  `Street Address` <- primary_address_street <- City <- primary_address_city <- State <- primary_address_state <- NULL
  Zip <- primary_address_postalcode <- Email <- email_address <- c2_contact_record_type_c <- key <- NULL

  if (type == "families") {
    key <- "Family"
  } else if (type == "people") {
    key <- "Person"
  } else {
    key <- "Organization"
  }

  tab <- get_contacts() %>%
    dplyr::filter(c2_contact_record_type_c == key) %>%
    dplyr::select(
      `Name` = contact2_full_name, `Phone` = c2_primary_phone,
      `Street Address` = primary_address_street, `City` = primary_address_city, `State` = primary_address_state,
      `Zip` = primary_address_postalcode, `Email` = email_address
    ) %>%
    dplyr::arrange(Name)

  ft <- goc_table(tab, "Contact Information", paste0(nrow(tab), " ", type))
  ft
}
