#' List Supports by Member and Type
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
#' list_supports_by_member_and_type()
list_supports_by_member_and_type <- function() {
  Member <- last_name <- first_name <- narrow <- n <- NULL

  supports <- get_supports(with.members = T) %>%
    dplyr::inner_join(dict_support_types(), by = c("flo_support_type" = "key")) %>%
    dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
    dplyr::group_by(Member, narrow) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = narrow, values_from = n)

  supports <- supports %>%
    dplyr::select("Member", sort(colnames(supports))) %>%
    dplyr::arrange(Member)

  ft <- goc_table(supports, "Supports by Member and Type")
  ft
}
