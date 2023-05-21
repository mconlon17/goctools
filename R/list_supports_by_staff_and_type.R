#' List Supports by Staff and Type
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider
#'
#' @return a flextable with rows for each member and columns for each support type
#' @export
#'
#' @examples
#' list_supports_by_staff_and_type()

list_supports_by_staff_and_type <- function() {
    
    Staff=last_name=first_name=narrow=n=NULL
    
    supports <- get_supports() %>%
        dplyr::inner_join(get_contacts(), by=c("sa_contacts_2_id"="id")) %>%
        dplyr::inner_join(dict_support_types(), by = c("flo_support_type" = "key")) %>%
        dplyr::mutate(Staff = paste0(last_name, ", ", first_name)) %>%
        dplyr::group_by(Staff, narrow) %>%
        dplyr::tally() %>%
        tidyr::pivot_wider(names_from=narrow, values_from=n)
    
    supports <- supports %>%
        dplyr::select("Staff", sort(colnames(supports))) %>%
        dplyr::arrange(Staff) 
    
    ft <- goc_table(supports, "Supports by Staff and Type")
    ft
    
}
