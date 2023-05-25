#' Check Contacts
#'
#' @param check One of "phone", "email", "street", "state", nl", "full_name", "zip", or "solicit" check corresponding column.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider
#'
#' @return a flextable with the check of the requested column
#' @export
#'
#' @examples
#' check_contacts()
#' check_contacts("phone")
check_contacts <- function(check = "state") {
  Type <- Solicit <- do_not_call <- c2_contact_record_type_c <- n <- NULL
  `Has Full Name` <- n <- contact2_full_name <- Solicit <- Type <- NULL
  `Contact ID` <- contact2_flourish_number <- `Street Address` <- primary_address_street <- NULL
  `Has State` <- primary_address_state <- NULL
  `Has Zip` <- primary_address_postalcode <- NULL
  `Has Street` <- primary_address_street <- NULL
  `Has Email` <- email_address <- NULL
  `Has Phone` <- c2_primary_phone <- NULL

  contacts <- get_contacts()

  if (check == "phone") {
    tab <- contacts %>%
      dplyr::mutate(`Has Phone` = ifelse(c2_primary_phone != "" & !is.na(c2_primary_phone) & c2_primary_phone != "NA", "Yes", "No")) %>%
      dplyr::group_by(`Has Phone`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has a Phone Number")
    ft
  }

  if (check == "email") {
    tab <- contacts %>%
      dplyr::mutate(`Has Email` = ifelse(!is.na(email_address), "Yes", "No")) %>%
      dplyr::group_by(`Has Email`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has an Email Address")
    ft
  }

  if (check == "street") {
    tab <- contacts %>%
      dplyr::mutate(`Has Street` = ifelse(primary_address_street != "NA" & primary_address_street != "" & !is.na(primary_address_street), "Yes", "No")) %>%
      dplyr::group_by(`Has Street`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has a Street Address")
    ft
  }

  if (check == "state") {
    tab <- contacts %>%
      dplyr::mutate(`Has State` = ifelse(primary_address_state != "NA" & primary_address_state != "" & !is.na(primary_address_state), "Yes", "No")) %>%
      dplyr::group_by(`Has State`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has a State")
    ft
  }

  if (check == "zip") {
    tab <- contacts %>%
      dplyr::mutate(`Has Zip` = ifelse(primary_address_postalcode != "NA" & primary_address_postalcode != "" & !is.na(primary_address_postalcode), "Yes", "No")) %>%
      dplyr::group_by(`Has Zip`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has a Zip Code")
    ft
  }

  if (check == "nl") {
    tab <- contacts %>%
      dplyr::filter(grepl("nl", primary_address_street)) %>%
      dplyr::select(`Contact ID` = contact2_flourish_number, `Street Address` = primary_address_street)

    ft <- goc_table(tab, paste0(nrow(tab), " Contact Records with 'nl' in address"))
    ft
  }

  if (check == "full_name") {
    tab <- contacts %>%
      dplyr::mutate(`Has Full Name` = ifelse(contact2_full_name != "NA" & contact2_full_name != "" & !is.na(contact2_full_name), "Yes", "No")) %>%
      dplyr::group_by(`Has Full Name`) %>%
      dplyr::tally()

    ft <- goc_table(tab, "Contact Record Has Full Name")
    ft
  }

  if (check == "solicit") {
    tab <- contacts %>%
      dplyr::rename(Solicit = do_not_call, Type = c2_contact_record_type_c) %>%
      dplyr::mutate(Solicit = ifelse(Solicit == 0, "No", "Yes")) %>%
      dplyr::group_by(Type, Solicit) %>%
      dplyr::tally() %>%
      tidyr::pivot_wider(names_from = Type, values_from = n)

    ft <- goc_table(tab, "Solicit")
    ft
  }

  ft
}
