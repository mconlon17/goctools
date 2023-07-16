#' List Gifts
#' 
#' @param date Gifts will be listed inclusive of the date, back two years
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom lubridate years
#' @importFrom lubridate days
#'
#' @return a flextable containing the gifts
#' @export
#'
#' @examples
#' list_gifts() # list gifts in the last year from today
#' list_gifts("2023-06-30") # list gifts in the last fiscal year
#' list_gifts("2022-12-31") # list gifts in the last calendar year

list_gifts <- function(date=Sys.Date()) {
  
  contact2_full_name=flo_gift_amount=flo_gift_date=NULL
  flo_gift_payment_method=id=name=value=NULL
    
  end_date <- lubridate::as_date(date)    
  start_date <- end_date - lubridate::years(1) + lubridate::days(1)

  gifts <- get_gifts() %>%
    dplyr::select(-id) %>%
    dplyr::left_join(as.data.frame(dict_campaign_names()), by = c("flo_gift_campaign" = "key")) %>%
    dplyr::left_join(get_contacts()[,c("id","contact2_full_name")], by = c("contact_2_id" = "id")) %>%
    dplyr::filter(flo_gift_date >= start_date & flo_gift_date <= end_date) %>%
    dplyr::arrange(desc(flo_gift_date)) %>%
    dplyr::select(`Gift Name`=name, `Date`=flo_gift_date, `Donor`=contact2_full_name, `Amount`=flo_gift_amount, Campaign=value,`Payment Method`=flo_gift_payment_method)

  ft <- goc_table(gifts, heading = "All Gifts In The Last Year", caption = paste0("Gifts from ", start_date, " to ", end_date)) %>%
      flextable::colformat_num(j = 4, prefix = "$")
  ft
}
