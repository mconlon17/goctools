#' Check Goals
#'
#' @param check Item to check.  Choices are: staff, member, text, lapsed_table, and lapsed_list
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom lubridate years
#'
#' @return a flextable with the checks
#' @export
#'
#' @examples
#' check_goals()
#' check_goals("lapsed_list") # a list of the lapsed goals
#' check_goals("lapsed_table") # a table counting lapsed goals by staff member
check_goals <- function(check = "lapsed_list") {
  `Creation Date` <- Goal <- Member <- Staff <- first_name <- flo_goal_creation_date <- NULL
  flo_goal_description <- flo_goal_end_date <- flo_goal_status <- flo_goal_type <- NULL
  last_name <- name <- n <- NULL

  goals <- get_goals() %>%
    dplyr::mutate(flo_goal_creation_date = lubridate::as_date(flo_goal_creation_date)) %>%
    dplyr::mutate(flo_goal_end_date = lubridate::as_date(flo_goal_end_date)) %>%
    dplyr::left_join(get_members()[, c("id", "first_name", "last_name")], by = c("contact_id" = "id")) %>%
    dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
    dplyr::mutate(flo_goal_creation_date = lubridate::as_date(flo_goal_creation_date)) %>%
    dplyr::select(-first_name, -last_name) %>%
    dplyr::left_join(get_contacts()[, c("id", "first_name", "last_name")], by = c("contact_2_id" = "id")) %>%
    dplyr::mutate(Staff = paste0(last_name, ", ", first_name)) %>%
    dplyr::select(
      `Goal Name` = name, `Creation Date` = flo_goal_creation_date, `End Date` = flo_goal_end_date,
      Goal = flo_goal_description, Type = flo_goal_type, Status = flo_goal_status, Staff, Member
    ) %>%
    dplyr::arrange(Staff, Member)

  if (check == "staff") {
    tab <- goals %>%
      dplyr::filter(Staff == "NA, NA")

    ft <- goc_table(tab, paste0(nrow(tab), " Goals with no Staff Member Assigned"))
    ft
  }

  if (check == "member") {
    tab <- goals %>%
      dplyr::filter(Member == "NA, NA")

    ft <- goc_table(tab, paste0(nrow(tab), " Goals with no Member Assigned"))
    ft
  }

  if (check == "text") {
    tab <- goals %>%
      dplyr::filter(is.na(Goal))

    ft <- goc_table(tab, paste0(nrow(tab), " Goals with no text"))
    ft
  }

  if (check == "lapsed_table") {
    tab <- goals %>%
      dplyr::filter(`Creation Date` < Sys.Date() - lubridate::years(1)) %>%
      dplyr::group_by(Staff) %>%
      dplyr::tally()

    ft <- goc_table(tab, paste0("Lapsed Goals -- over 1 year since creation -- by Staff Member"))
    ft
  }

  if (check == "lapsed_list") {
    tab <- goals %>%
      dplyr::filter(`Creation Date` < Sys.Date() - lubridate::years(1))

    ft <- goc_table(goals, paste0(nrow(tab), " Lapsed Goals -- over 1 year since creation"))
    ft
  }
  ft
}
