#' Check for Duplicate Members
#'
#' Members are checked for closeness measured by Levenshtein distance on last name, first name. 0 indicates two names are
#' identical,
#' 1 indicates a single character edit (addition, deletion, or change) will make two names identical.
#'
#' @param closeness members with names within Levenshtein closeness to include on report
#' @importFrom stringdist stringdistmatrix
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @return a flextable showing closeness of member last name, first name to other member's last name, first name
#' @export
#'
#' @examples
#' check_duplicate_members()
#' check_duplicate_members(4) # relax the closeness for a longer list
check_duplicate_members <- function(closeness = 3) {
  `How close` <- `This member name is close to that member name` <- last_name <- first_name <- NULL

  members <- get_members() %>%
    mutate(Name = paste0(last_name, ", ", first_name))

  X <- as.matrix(stringdist::stringdistmatrix(members$Name))
  diag(X) <- Inf
  rownames(X) <- members$Name
  colnames(X) <- members$Name

  result <- t(sapply(seq(nrow(X)), function(i) {
    j <- which.min(X[i, ])
    c(paste(rownames(X)[i], colnames(X)[j], sep = " / "), X[i, j])
  }))
  colnames(result) <- c("This member name is close to that member name", "How close")
  result <- as.data.frame(result) %>%
    dplyr::mutate(`How close` = as.numeric(`How close`)) %>%
    dplyr::filter(`How close` <= closeness) %>%
    dplyr::arrange(`How close`, `This member name is close to that member name`)
  goc_table(result, heading = "Check for Duplicate Members", caption = "Closeness of Member last name, first names")
}
