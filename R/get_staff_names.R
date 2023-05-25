#' Get names of staff members
#'
#' @return a vector of staff names.  Each name is "last, first"
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @export
#'
#' @examples
#' length(get_staff_names()) # count number of staff
get_staff_names <- function() {
  c2_groups <- name <- last_name <- first_name <- NULL

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Staff", c2_groups)) %>%
    dplyr::mutate(name = paste0(last_name, ", ", first_name)) %>%
    dplyr::select(name) %>%
    dplyr::arrange(name)

  contacts$name
}
