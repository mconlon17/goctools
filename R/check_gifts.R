#' Check Gifts
#' @param check The check to be performed, one of: donor, restriction, anonymous, amount, campaign, date, method, restricted, or thank_you
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' 
#' @return a flextable with the requested gift checks
#' @export
#'
#' @examples
#' check_gifts()
#' check_gifts("restriction")

check_gifts <- function(check="donor") {
    
    contact_2_id=`Gift ID`=name=Date=flo_gift_date=Amount=flo_gift_amount=Method=flo_gift_payment_method=Campaign=flo_gift_campaign=NULL
    Letter=GOC_restriction_letter=NULL
    Anonymous=flo_anonymous_gift=n=NULL
    Amount=flo_gift_amount=n=NULL
    Campaign=flo_gift_campaign=n=NULL
    Year=flo_gift_date=n=NULL
    Method=flo_gift_payment_method=NULL
    Restricted=flo_restricted_gift=NULL
    `Thank you date`=flo_thank_you_date=n=NULL
    
    gifts <- get_gifts()
    
    if(check=="donor") {
        tab <- gifts %>%
            dplyr::filter(is.na(contact_2_id)) %>%
            dplyr::select(`Gift ID`=name, Date=flo_gift_date, Amount=flo_gift_amount, Method=flo_gift_payment_method, Campaign=flo_gift_campaign)
        
        ft <- goc_table(tab, paste0(nrow(tab), " Gifts with no donor")) %>% colformat_num(j=3,bigmark=",",prefix="$")
        ft
    }
    
    if(check=="restriction") {
        tab <- gifts %>%
            dplyr::filter(!is.na(GOC_restriction_letter) & GOC_restriction_letter != "" & GOC_restriction_letter != "http://") %>%
            dplyr::select(`Gift ID`=name, Date=flo_gift_date, Amount=flo_gift_amount, Method=flo_gift_payment_method, 
                          Campaign=flo_gift_campaign, Letter=GOC_restriction_letter)
        
        ft <- goc_table(tab, paste0(nrow(tab), " Gifts with restriction letters")) %>% colformat_num(j=3,bigmark=",",prefix="$")
        ft
    }
    
    if(check=="anonymous") {
        tab <- gifts %>%
            dplyr::group_by(flo_anonymous_gift) %>%
            dplyr::tally() %>%
            dplyr::select(Anonymous=flo_anonymous_gift,n)
        
        ft <- goc_table(tab, "Anonymous Gift")
        ft
    }
    
    if(check=="amount") {
        tab <- gifts %>%
            dplyr::group_by(flo_gift_amount) %>%
            dplyr::tally() %>%
            dplyr::select(Amount=flo_gift_amount,n)
        
        ft <- goc_table(tab, "Gift Amounts") %>% colformat_num(j=1,bigmark=",",prefix="$")
        ft
    }
    
    if(check=="campaign") {
        tab <- gifts %>%
            dplyr::group_by(flo_gift_campaign) %>%
            dplyr::tally() %>%
            dplyr::select(Campaign=flo_gift_campaign,n)
        
        ft <- goc_table(tab, "Gifts by Campaign")
        ft
    }
    
    if(check=="date") {
        tab <- gifts %>%
            dplyr::mutate(Year = lubridate::year(lubridate::as_date(flo_gift_date))) %>%
            dplyr::group_by(Year) %>%
            dplyr::tally()
        
        ft <- goc_table(tab, "Gifts by Year")
        ft
    }
    
    if(check=="method") {
        tab <- gifts %>%
            dplyr::group_by(flo_gift_payment_method) %>%
            dplyr::tally() %>%
            dplyr::select(Method=flo_gift_payment_method,n)
        
        ft <- goc_table(tab, "Gifts by Payment Method")
        ft
    }
    
    if(check=="restricted") {
        tab <- gifts %>%
            dplyr::group_by(flo_restricted_gift) %>%
            dplyr::tally() %>%
            dplyr::select(Restricted=flo_restricted_gift,n)
        
        ft <- goc_table(tab, "Restricted Gift")
        ft
    }
    
    if(check=="thank_you") {
        tab <- gifts %>%
            dplyr::group_by(flo_thank_you_date) %>%
            dplyr::tally() %>%
            dplyr::select("Thank you date"=flo_thank_you_date,n)
        
        ft <- goc_table(tab, "Thank You Dates")
        ft
    }
    
    ft

}
