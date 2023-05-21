#' List Members By City
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr full_join
#' @importFrom dplyr select
#'
#' @return a flextable with one row per city, including count of members and county of city
#' @export
#'
#' @examples
#' list_members_by_city()

list_members_by_city <- function() {
    
    n=primary_address_city=value=NULL
    
    members <- get_members() %>%
        dplyr::group_by(primary_address_city) %>%
        dplyr::tally() %>%
        dplyr::full_join(dict_city_county(), by=c("primary_address_city"="key")) %>%
        dplyr::select(City=primary_address_city, n, County=value)
    
    ft <- goc_table(members, "Members By City")
    ft
}