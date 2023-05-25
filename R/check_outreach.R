#' Check Outreach
#'
#' @param check The check to be performed.  Choices are member, date, type, or outcome
#'
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#'
#' @return a flextable with the results of the requested check
#' @export
#'
#' @examples
#' check_outreach()
#' check_outreach("type")
check_outreach <- function(check = "member") {
  contact_id <- `Outreach ID` <- name <- Date <- ro_date_of_reach_out <- Type <- NULL
  ro_type_of_reachout <- `Outreach Outcome` <- ro_outcome_of_ro <- n <- NULL

  outreach <- get_outreach()

  if (check == "member") {
    tab <- outreach %>%
      dplyr::filter(is.na(contact_id) | contact_id == "") %>%
      dplyr::arrange(desc(ro_date_of_reach_out)) %>%
      dplyr::select(`Outreach ID` = name, Date = ro_date_of_reach_out)

    ft <- goc_table(tab, paste0(nrow(tab), " Outreach records without member"))
    ft
  }

  if (check == "date") {
    tab <- outreach %>%
      dplyr::filter(is.na(ro_date_of_reach_out) | ro_date_of_reach_out == "") %>%
      dplyr::select(`Outreach ID` = name)

    ft <- goc_table(tab, paste0(nrow(tab), " Outreach records without date"))
    ft
  }

  if (check == "type") {
    tab <- outreach %>%
      dplyr::group_by(ro_type_of_reachout) %>%
      dplyr::tally() %>%
      dplyr::select(Type = ro_type_of_reachout, n)

    ft <- goc_table(tab, "Outreach Type")
    ft
  }

  if (check == "outcome") {
    tab <- outreach %>%
      dplyr::group_by(ro_outcome_of_ro) %>%
      dplyr::tally() %>%
      dplyr::select(`Outreach Outcome` = ro_outcome_of_ro, n)

    ft <- goc_table(tab, "Outreach Outcome")
    ft
  }

  ft
}
