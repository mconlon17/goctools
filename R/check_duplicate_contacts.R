#' Check for Duplicate Contacts
#' 
#' Contacts are checked for closeness measured by Levenshtein distance on full name. 0 indicates two full names are
#' identical, 
#' 1 indicates a single character edit (addition, deletion, or change) will make two full names identical.
#' 
#' @param closeness contacts with full names within Levenshtein closeness to include on report
#' @importFrom stringdist stringdistmatrix
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @return a flextable showing closeness of contact full names to other contacts full names
#' @export
#'
#' @examples
#' check_duplicate_contacts() 
#' check_duplicate_contacts(4) # relax the closeness for a longer list

check_duplicate_contacts <- function(closeness=3) {
    
    `How close`=`This contact name is close to that contact name`=NULL
    
    contacts <- get_contacts()[,"contact2_full_name"]
    
    X <- as.matrix(stringdist::stringdistmatrix(contacts))
    diag(X) <- Inf
    rownames(X) <- contacts
    colnames(X) <- contacts
    
    result <- t(sapply(seq(nrow(X)), function(i) {
        j <- which.min(X[i, ])
        c(paste(rownames(X)[i], colnames(X)[j], sep = " / "), X[i, j])
    }))
    colnames(result) <- c("This contact name is close to that contact name", "How close")
    result <- as.data.frame(result) %>%
        dplyr::mutate(`How close` = as.numeric(`How close`)) %>%
        dplyr::filter(`How close` <= closeness) %>%
        dplyr::arrange(`How close`, `This contact name is close to that contact name`)
    goc_table(result, heading="Check for Duplicate Contacts", caption="Closeness of contact full names")
}
