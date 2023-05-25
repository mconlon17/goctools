#' Gift Amounts by Campaign
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @return a flextable with rows for each campaign, and columns for gift sizes
#' @export
#'
#' @examples
#' list_gift_amounts_by_campaign()
list_gift_amounts_by_campaign <- function() {
  Amount <- Campaign <- flo_gift_amount <- flo_gift_campaign <- NULL
  flo_gift_payment_method <- n <- NULL

  dict <- dict_campaign_names()
  values <- dict$value
  keys <- dict$key

  gifts <- get_gifts()

  tab <- gifts %>%
    dplyr::filter(flo_gift_payment_method != "in_kind") %>%
    dplyr::filter(!(flo_gift_amount %in% c(1030.18, 3508.94, 6500, 365.91, 421.21))) %>%
    dplyr::mutate(Amount = as.character(flo_gift_amount)) %>%
    dplyr::mutate(Amount = ifelse(0 <= flo_gift_amount & flo_gift_amount <= 10, "    $1-$10", Amount)) %>%
    dplyr::mutate(Amount = ifelse(11 <= flo_gift_amount & flo_gift_amount <= 49, "   $11-$49", Amount)) %>%
    dplyr::mutate(Amount = ifelse(50 == flo_gift_amount, "   $50", Amount)) %>%
    dplyr::mutate(Amount = ifelse(51 <= flo_gift_amount & flo_gift_amount <= 99, "   $51-$99", Amount)) %>%
    dplyr::mutate(Amount = ifelse(101 <= flo_gift_amount & flo_gift_amount <= 199, "  $101-$199", Amount)) %>%
    dplyr::mutate(Amount = ifelse(100 == flo_gift_amount, "  $100", Amount)) %>%
    dplyr::mutate(Amount = ifelse(200 == flo_gift_amount, "  $200", Amount)) %>%
    dplyr::mutate(Amount = ifelse(250 == flo_gift_amount, "  $250", Amount)) %>%
    dplyr::mutate(Amount = ifelse(251 <= flo_gift_amount & flo_gift_amount <= 499, "  $251-$499", Amount)) %>%
    dplyr::mutate(Amount = ifelse(500 == flo_gift_amount, "  $500", Amount)) %>%
    dplyr::mutate(Amount = ifelse(501 <= flo_gift_amount & flo_gift_amount <= 999, "  $501-$999", Amount)) %>%
    dplyr::mutate(Amount = ifelse(1000 == flo_gift_amount, " $1000", Amount)) %>%
    dplyr::mutate(Amount = ifelse(1500 == flo_gift_amount, " $1500", Amount)) %>%
    dplyr::mutate(Amount = ifelse(2000 == flo_gift_amount, " $2000", Amount)) %>%
    dplyr::mutate(Amount = ifelse(2500 == flo_gift_amount, " $2500", Amount)) %>%
    dplyr::mutate(Amount = ifelse(3000 == flo_gift_amount, " $3000", Amount)) %>%
    dplyr::mutate(Amount = ifelse(5000 == flo_gift_amount, " $5000", Amount)) %>%
    dplyr::mutate(Amount = ifelse(15000 == flo_gift_amount, "$15000", Amount)) %>%
    dplyr::group_by(flo_gift_campaign, Amount) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = Amount, values_from = n) %>%
    dplyr::mutate(Campaign = values[which(keys == flo_gift_campaign)]) %>%
    dplyr::ungroup() %>%
    dplyr::select(Campaign, dplyr::contains("$"))

  ft <- goc_table(tab, "Gift Amounts by Campaign")
  ft
}
