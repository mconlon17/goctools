#' Goals by Active Member and Staff
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom tidyr pivot_wider
#'
#' @return a flextable showing goals for each member by staff
#'
#' @export
#'
#' @examples
#' list_goals_by_member_and_staff()
list_goals_by_member_and_staff <- function() {
  last_name <- first_name <- Member <- Staff <- n <- NULL

  goals <- get_goals() %>%
    dplyr::inner_join(get_members(active.only = T)[, c("id", "first_name", "last_name")], by = c("contact_id" = "id")) %>%
    dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
    dplyr::select(-first_name, -last_name) %>%
    dplyr::left_join(get_contacts()[, c("id", "first_name", "last_name")], by = c("contact_2_id" = "id")) %>%
    dplyr::mutate(Staff = paste0(first_name, " ", last_name)) %>%
    dplyr::group_by(Member, Staff) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = Staff, values_from = n)

  ft <- goc_table(goals, "Goals by Active Member and Staff",
    caption = "NA NA indicates goals not assigned to Staff Members\nFormer Staff are included when goals are assigned to them"
  )
  ft
}
