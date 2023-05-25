#' Supports by Year
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr tally
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate year
#'
#' @return a flextable showing support types (rows) and years (columns).  Entries are the counts of supports of that type for the year.
#' @export
#'
#' @examples
#' list_supports_by_year()
list_supports_by_year <- function() {
  Year <- flo_support_date <- flo_support_type <- value <- NULL

  supports <- get_supports() %>%
    dplyr::mutate(Year = lubridate::year(flo_support_date)) %>%
    dplyr::select(flo_support_type, Year) %>%
    dplyr::inner_join(dict_support_types(), by = c("flo_support_type" = "key")) %>%
    dplyr::group_by(value, Year) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = Year, values_from = n) %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
    dplyr::ungroup()

  supports <- supports %>%
    dplyr::select(`Support Type` = value, order(colnames(supports)))

  supports <- dplyr::bind_rows(supports, dplyr::summarise(
    supports, dplyr::across(where(is.numeric), sum),
    dplyr::across(where(is.character), ~"Total")
  ))

  supports[supports == 0] <- NA

  ft <- goc_table(supports, "Supports Types By Year")
  ft
}
