#' List Employment Supports
#'
#' @param date Supports will be listed for 90 days up to and including the specified date
#'
#' @importFrom lubridate as_date
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join


#' @return a flextable with the desired employment supports
#' @export
#'
#' @examples
#' list_employment_supports()
#' list_employment_supports("2022-12-31")

list_employment_supports <- function(date = Sys.Date()) {
    
    Date=Member=first_name=flo_support_date=flo_support_note=NULL
    flo_support_type=id=last_name=NULL
    
    members <- get_members() %>%
        dplyr::mutate(Member = paste0(last_name, ", ", first_name))
    
    employment.supports <- get_supports() %>%
        dplyr::filter(flo_support_date >= lubridate::as_date(date) - lubridate::days(90) & flo_support_date <= lubridate::as_date(date)) %>%
        dplyr::filter(flo_support_type == "Employment_readiness" |
                   flo_support_type == "Employment_IE" |
                   flo_support_type == "Employment_SE" |
                   flo_support_type == "Employment_TE") %>%
        dplyr::select(id, flo_support_date, flo_support_type, flo_support_note)
    
    support.rel <- get_table("flo_supports_contacts_c") %>%
        dplyr::select("flo_supports_contactsflo_supports_ida", "flo_supports_contactscontacts_idb")
    
    tab <- members %>%
        dplyr::inner_join(support.rel, by = c("id" = "flo_supports_contactscontacts_idb")) %>%
        dplyr::inner_join(employment.supports, by = c("flo_supports_contactsflo_supports_ida" = "id")) %>%
        dplyr::select(Member, Date = flo_support_date, Type = flo_support_type, Note = flo_support_note) %>%
        dplyr::arrange(Member, Date)
    
    ft <- goc_table(tab, "Members with employment supports", paste0("from ", lubridate::as_date(date) - lubridate::days(90), " to ", date))
    ft
}
