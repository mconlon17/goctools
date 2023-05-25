#' Members By Year
#'
#' @param output If "flextable" a flextable is returned with one row per year and the number of members attending in that year counted.  Otherwise a data.frame is returned.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr select
#'
#' @return either a flextable or a data.frame, depending on the value of the output parameter.  One row per year, count of the number of members attending at least once in that year.
#' @export
#'
#' @examples
#' list_members_by_year()
list_members_by_year <- function(output = "flextable") {
  da_date_of_attendance <- Year <- c2_groups <- contact_2_id <- `Number of Members` <- n <- NULL

  attendance <- get_attendance() %>%
    dplyr::mutate(Year = year(da_date_of_attendance)) %>%
    dplyr::left_join(get_contacts(), by = c("contact_2_id" = "id")) %>%
    dplyr::filter(grepl("Member", c2_groups)) %>%
    dplyr::group_by(Year, contact_2_id) %>%
    dplyr::tally() %>%
    dplyr::group_by(Year) %>%
    dplyr::tally() %>%
    dplyr::select(Year, "Number of Members" = n)

  if (output == "flextable") {
    ft <- goc_table(attendance, "Members by year")
    ft
  } else {
    attendance
  }
}
