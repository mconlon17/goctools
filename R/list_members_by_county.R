#' List Members By County
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr full_join
#' @importFrom dplyr select
#'
#' @return a flextable with one row per county, including count of members
#' @export
#'
#' @examples
#' list_members_by_county()

list_members_by_county <- function() {
    
    n=primary_address_city=value=County=NULL
    
    members <- get_members() %>%
        dplyr::group_by(primary_address_city) %>%
        dplyr::tally() %>%
        dplyr::full_join(dict_city_county(), by=c("primary_address_city"="key")) %>%
        dplyr::select(County=value,n) %>%
        dplyr::group_by(County) %>%
        dplyr::summarise(n=sum(n))
    
    ft <- goc_table(members, "Members By County")
    ft
}