#' List All Donors
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr inner_join
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#'
#' @return a flex table listing all donors and total dollars donated
#' @export
#'
#' @examples
#' list_all_donors()
list_all_donors <- function() {
  c2_primary_phone <- contact2_full_name <- contact_2_id <- NULL
  email_address <- flo_gift_amount <- primary_address_city <- NULL
  primary_address_postalcode <- primary_address_state <- NULL
  primary_address_street <- total_dollars <- NULL

  gift.amounts <- get_gifts() %>%
    dplyr::group_by(contact_2_id) %>%
    dplyr::summarise(total_dollars = sum(flo_gift_amount))

  donors <- get_contacts() %>%
    dplyr::inner_join(gift.amounts, by = c("id" = "contact_2_id")) %>%
    dplyr::arrange(dplyr::desc(total_dollars)) %>%
    dplyr::select(
      `Name` = contact2_full_name, `Total Dollars` = total_dollars, `Phone` = c2_primary_phone,
      `Street Address` = primary_address_street, `City` = primary_address_city,
      `State` = primary_address_state, `Zip` = primary_address_postalcode, `Email` = email_address
    )

  ft <- goc_table(donors, heading = "All Donors All time") %>% flextable::colformat_num(j = 2, bigmark = ",", prefix = "$")
  ft
}
