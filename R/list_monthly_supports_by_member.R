#' Monthly Supports by Member
#'
#' @param date A date in the month the table will be made.  For example if date = "2023-04-12", the table will be made for April, 2023.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom lubridate "%within%"
#' @importFrom lubridate "%--%"
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate days
#' @importFrom tidyr pivot_wider
#' @importFrom flextable width
#'
#' @return a flextable with one row per member attending in the selected month, columns for support types and cells with counts of the support type for each member
#' @export
#'
#' @examples
#' list_monthly_supports_by_member()
#' list_monthly_supports_by_member("2023-04-12")
list_monthly_supports_by_member <- function(date = Sys.Date()) {
  Name <- first_name <- flo_support_date <- flo_support_type <- last_name <- NULL

  date <- lubridate::as_date(date)
  start_date <- lubridate::floor_date(date, "month")
  end_date <- lubridate::ceiling_date(date, "month") - lubridate::days(1)

  types <- dict_support_types()

  supports <- get_supports(with.members = T) %>%
    dplyr::mutate(Name = paste0(last_name, ", ", first_name)) %>%
    dplyr::mutate(flo_support_date = as_date(flo_support_date)) %>%
    dplyr::filter(flo_support_date %within% (start_date %--% end_date)) %>%
    dplyr::select(flo_support_type, flo_support_date, Name) %>%
    dplyr::arrange(Name)

  tab <- supports %>%
    dplyr::group_by(Name, flo_support_type) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = flo_support_type, values_from = n) %>%
    dplyr::ungroup()

  x <- match(colnames(tab), types$key)
  colnames(tab)[seq(2, length(x))] <- types$narrow[x[seq(2, length(x))]]

  tab[is.na(tab)] <- 0

  tab <- dplyr::bind_rows(tab, summarise(
    tab, dplyr::across(where(is.numeric), sum),
    dplyr::across(where(is.character), ~"Total")
  ))

  tab <- cbind(tab, data.frame(Total = rowSums(tab[, -1])))

  tab[tab == 0] <- NA

  ft <- goc_table(tab, paste0("Member Supports for ", format(date, "%B %Y")),
    caption = paste0(tab[nrow(tab), ncol(tab)], " supports for ", nrow(tab), " members")
  ) %>%
    flextable::width(j = 1, width = 2)
  ft
}
