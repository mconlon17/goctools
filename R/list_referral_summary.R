#' List Referring Agencies
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr row_number
#'
#' @return a flextable of agencies, the number of referrals they have made, and the date of last referral
#' @export
#'
#' @examples
#' list_referral_summary()

list_referral_summary <- function() {
    
    Referrals=`Most Recent`=n=Name=contact2_full_name=flo_application_date=id=referral_source_id_c=NULL
    
    referrals <- get_members() %>%
        dplyr::filter(!is.na(referral_source_id_c) & referral_source_id_c != "") %>%
        dplyr::select(id, referral_source_id_c, flo_application_date)
    
    referral.count <- referrals %>%
        dplyr::group_by(referral_source_id_c) %>%
        dplyr::tally()
    
    referral.recent <- referrals %>%
        dplyr::group_by(referral_source_id_c) %>%
        dplyr::arrange(desc(flo_application_date)) %>%
        dplyr::filter(dplyr::row_number() == 1)
    
    referrals <- referral.count %>%
        dplyr::left_join(referral.recent, by = "referral_source_id_c")
    
    contacts <- get_contacts() %>%
        dplyr::select(id, contact2_full_name)
    
    referrals <- referrals %>%
        dplyr::inner_join(contacts, by = c("referral_source_id_c"="id")) %>%
        dplyr::select(Name = contact2_full_name, Referrals = n, `Most Recent` = flo_application_date ) %>%
        arrange(Name)
    
    ft <- goc_table(referrals, "Referring Agencies")
    ft
}