#' List Goal Types by Staff
#'
#' @param date Date around which the unit will be calculated
#' @param unit Time period to be displayed from week, month, year, all
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#'
#' @return a flextable showing goal types by staff for the desired time from the request date
#' @export
#'
#' @examples
#' list_goal_types_by_staff()
#' list_goal_types_by_staff(date = "2023-01-01", unit = "week")
list_goal_types_by_staff <- function(date = Sys.Date(), unit = "all") {
  start_date <- end_date <- Year <- Name <- flo_goal_creation_date <- flo_goal_type <- n <- `Goal Type` <- first_name <- last_name <- flo_goal_type <- NULL

  if (unit == "all") {
    start_date <- lubridate::as_date(-Inf)
    end_date <- lubridate::as_date(Inf)
  } else {
    start_date <- lubridate::floor_date(lubridate::as_date(date), unit)
    end_date <- min(Sys.Date(), lubridate::ceiling_date(lubridate::as_date(date), unit) - 1)
  }

  goals <- get_goals() %>%
    dplyr::mutate(Year = year(lubridate::as_date(flo_goal_creation_date))) %>%
    dplyr::inner_join(get_contacts(), by = c("contact_2_id" = "id")) %>%
    dplyr::mutate(Name = paste0(last_name, ", ", first_name)) %>%
    dplyr::filter(lubridate::as_date(flo_goal_creation_date) %within% (start_date %--% end_date)) %>%
    dplyr::group_by(flo_goal_type, Name) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = Name, values_from = n) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
    dplyr::rename(`Goal Type` = flo_goal_type)

  ft <- goc_table(goals, ifelse(unit == "all", "Goal Types By Staff All Time", paste0("Goal Types By Staff from ", start_date, " to ", end_date)))
  ft
}
