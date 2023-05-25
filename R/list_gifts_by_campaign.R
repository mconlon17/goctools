#' List Gifts by Campaign
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom lubridate year
#' @importFrom lubridate as_date
#'
#' @return a flextable listing the number of gifts and donors for each campaign
#' @export
#'
#' @examples
#' list_gifts_by_campaign()
list_gifts_by_campaign <- function() {
  Year <- flo_gift_date <- Count <- flo_gift_campaign <- `Number of Donations` <- `Number of Donors` <- NULL
  contact_2_id <- Total <- flo_gift_amount <- flo_gift_campaign <- Campaign <- NULL

  gifts <- get_gifts() %>%
    dplyr::mutate(Year = lubridate::year(lubridate::as_date(flo_gift_date))) %>%
    dplyr::mutate(Count = 1) %>%
    dplyr::group_by(flo_gift_campaign) %>%
    dplyr::summarise(`Number Of Donations` = sum(Count), `Number of Donors` = n_distinct(contact_2_id), Total = sum(flo_gift_amount)) %>%
    dplyr::arrange(dplyr::desc(flo_gift_campaign)) %>%
    dplyr::rename(Campaign = flo_gift_campaign)

  ft <- goc_table(gifts, "Gifts by Campaign") %>% colformat_num(j = "Total", big.mark = ",", prefix = "$")
  ft
}
