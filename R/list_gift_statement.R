#' List Gift Statement
#'
#' @param contact_id The Contact ID of the contact whose statement is need
#' @param year The year for which the statement is needed
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom flextable set_formatter_type
#' @importFrom flextable colformat_num
#'
#' @return a flextable containing the statement
#' @export
#'
#' @examples
#' list_gift_statement("Contact-03914", 2022)
#' list_gift_statement("Contact-04044", 2022)

list_gift_statement <- function(contact_id, year) {
    
    Amount=Date=contact2_flourish_number=contact2_full_name=NULL
    flo_gift_amount=flo_gift_date=flo_gift_payment_method=id=NULL
    
    contact <- get_contacts(include.email=F) %>%
        dplyr::filter(contact2_flourish_number == contact_id) %>%
        dplyr::select(id, contact2_full_name) %>%
        dplyr::left_join(get_gifts(), by = c("id" = "contact_2_id")) %>%
        dplyr::filter(lubridate::year(flo_gift_date) == year)
    
    name <- contact[1,"contact2_full_name"]
    total <- sum(contact[, "flo_gift_amount"])
    
    contact <- contact %>%
        dplyr::select(Date = flo_gift_date, Amount = flo_gift_amount, Payment = flo_gift_payment_method) %>%
        dplyr::mutate(Amount = round(Amount, 2)) %>%
        dplyr::arrange(Date) %>%
        rbind(data.frame(Date = "Total", Amount = total, Payment = ""))
    
    ft <- goc_table(contact, heading=name, caption=as.character(year)) %>%
        flextable::set_formatter_type(fmt_double = "%.2f") %>%
        flextable::colformat_num(j = 2, big.mark = ",", prefix = "$", nsmall = 2)
    ft
}
