#' Get contact data from Flourish
#'
#' @param include.deleted if TRUE, deleted contacts are included
#' @param include.email if TRUE, the primary emaiil address for the contact is included in the data returned from Flourish
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#'
#' @return a data.frame containing contact data
#' @export
#'
#' @examples
#' get_contacts()
#' contacts <- get_contacts(include.email = TRUE)
#' nrow(contacts[!is.na(contacts$email_address), ]) # count records with email
get_contacts <- function(include.deleted = FALSE, include.email = T) {
  bean_id <- bean_module <- date_created <- date_modified.x <- date_modified.y <- NULL
  deleted <- deleted.x <- deleted.y <- email_address <- email_address_id <- id <- id.y <- NULL
  primary_address <- reply_to_address <- NULL

  contacts <- get_table("sa_contacts_2", include.deleted)

  if (include.email) {
    email.addr.bean.rel <- get_table("email_addr_bean_rel") %>% dplyr::filter(primary_address == 1)

    if (!include.deleted) {
      email.addr.bean.rel <- email.addr.bean.rel %>% dplyr::filter(!deleted)
    }

    email.addr.bean.rel %>% dplyr::select(bean_id, email_address_id)

    email.addresses <- get_table("email_addresses")

    if (!include.deleted) {
      email.addresses <- email.addresses %>% dplyr::filter(!deleted)
    }

    email.addresses <- email.addresses %>% dplyr::select(id, email_address)

    contacts <- contacts %>%
      dplyr::left_join(email.addr.bean.rel, by = c("id" = "bean_id")) %>%
      dplyr::left_join(email.addresses, by = c("email_address_id" = "id")) %>%
      dplyr::rename(date_modified = date_modified.x, deleted = deleted.x) %>%
      dplyr::select(
        -id.y, -email_address_id, -bean_module, -primary_address,
        -reply_to_address, -date_modified.y, -deleted.y, -date_created
      )
  }

  contacts
}
