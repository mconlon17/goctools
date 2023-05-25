#' List Campaign donors
#'
#' Mutliple gifts by a donor to a campaign are totaled
#'
#' @param gift_campaign value for campaign, for example "2018 Amazing Give"
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom flextable colformat_num
#'
#' @return a flextable showing donors to the selected campaign
#' @export
#'
#' @examples
#' list_campaign_donors()
#' list_campaign_donors("2022 Annual Campaign")
list_campaign_donors <- function(gift_campaign = "2022 Amazing Give") {
  campaigns <- flo_gift_campaign <- contact_2_id <- Amount <- flo_gift_amount <- Donor <- contact2_full_name <- NULL

  campaigns <- dict_campaign_names()
  gift_campaign_key <- campaigns$key[which(campaigns$value == gift_campaign)]

  gifts <- get_gifts() %>%
    dplyr::filter(flo_gift_campaign == gift_campaign_key) %>%
    dplyr::group_by(contact_2_id) %>%
    dplyr::summarise(Amount = sum(flo_gift_amount)) %>%
    dplyr::arrange(desc(Amount)) %>%
    dplyr::left_join(get_contacts()[, c("id", "contact2_full_name")], by = c("contact_2_id" = "id")) %>%
    dplyr::select(Donor = contact2_full_name, Amount)

  ft <- goc_table(gifts, paste0(nrow(gifts), " Donors to ", gift_campaign)) %>% flextable::colformat_num(j = 2, bigmark = ",", prefix = "$")
  ft
}
