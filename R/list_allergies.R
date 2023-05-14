#' List Member Allergies
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom stringr str_detect

#' @return a flextable list of members and their allergies
#' @export
#'
#' @examples
#' list_allergies()

list_allergies <- function() {
    
    flo_allergies=last_name=first_name=NULL
    
    allergies <- get_members() %>%
        dplyr::filter(!stringr::str_detect(flo_allergies, "unknown") & !stringr::str_detect(flo_allergies, "none") & 
                          flo_allergies != "" & !is.na(flo_allergies)) %>%
        dplyr::mutate(flo_allergies = gsub(",", "A", flo_allergies)) %>%
        dplyr::mutate(flo_allergies = gsub("_", "B", flo_allergies)) %>%
        dplyr::mutate(flo_allergies = gsub("[[:punct:]]", "", flo_allergies)) %>%
        dplyr::mutate(flo_allergies = gsub("A", ", ", flo_allergies)) %>%
        dplyr::mutate(flo_allergies = gsub("B", "_", flo_allergies)) %>%
        dplyr::arrange(last_name, first_name) %>%
        dplyr::select(`First Name` = first_name, `Last Name` = last_name, `Allergies` = flo_allergies)
    
    ft <- goc_table(allergies, "Member Allergies")
    ft
}
