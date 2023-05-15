#' List Veterans
#'
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select

#' @return a flextable of members who are veterans
#' @export
#'
#' @examples
#' list_veterans()

list_veterans <- function() {
    
    flo_considered_veteran_c=last_name=first_name=`First Name`=`Last Name`=NULL
    
    veterans <- get_members() %>%
        dplyr::filter(flo_considered_veteran_c == "Yes") %>%
        dplyr::arrange(last_name, first_name) %>%
        dplyr::select(`First Name` = first_name, `Last Name` = last_name, `Veteran` = flo_considered_veteran_c)
    
    ft <- goc_table(veterans, paste0(nrow(veterans), " Veterans"))
    ft
}
