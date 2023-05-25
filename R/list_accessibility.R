#' List Accessibility Restrictions of Members
#'
#' @return a flextable report of member accessibility restrictions
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' list_accessibility()
list_accessibility <- function() {
  last_name <- first_name <- flo_accessibility_restrictions <- NULL

  accessibility <- get_members() %>%
    dplyr::filter(!stringr::str_detect(flo_accessibility_restrictions, "unknown") &
      !stringr::str_detect(flo_accessibility_restrictions, "none") &
      flo_accessibility_restrictions != "" & !is.na(flo_accessibility_restrictions)) %>%
    dplyr::mutate(flo_accessibility_restrictions = gsub(",", "A", flo_accessibility_restrictions)) %>%
    dplyr::mutate(flo_accessibility_restrictions = gsub("_", "B", flo_accessibility_restrictions)) %>%
    dplyr::mutate(flo_accessibility_restrictions = gsub("[[:punct:]]", "", flo_accessibility_restrictions)) %>%
    dplyr::mutate(flo_accessibility_restrictions = gsub("A", ", ", flo_accessibility_restrictions)) %>%
    dplyr::mutate(flo_accessibility_restrictions = gsub("B", "_", flo_accessibility_restrictions)) %>%
    dplyr::arrange(last_name, first_name) %>%
    dplyr::select(
      `First Name` = first_name, `Last Name` = last_name,
      `Accessibility Restrictions` = flo_accessibility_restrictions
    )

  ft <- goc_table(accessibility, "Member Accessibility Restrictions")
  ft
}
