#' Current Employment
#'
#' Must have support as one of IE, TE, SE, and no end date
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#'
#' @return a flextable showing the current employment of members
#' @export
#'
#' @examples
#' list_current_employment()
list_current_employment <- function() {
  flo_emp_date_end <- flo_emp_job_type <- Member <- NULL
  `GOC Support` <- `Job Title` <- flo_emp_job_title <- Employer <- contact2_full_name <- NULL
  `Start Date` <- flo_emp_date_start <- first_name <- last_name <- NULL
  `Hourly Pay` <- flo_emp_approximate_hourly_pay <- `Hours/Week` <- flo_emp_approximate_hours_wk <- NULL

  emp <- get_employment() %>%
    dplyr::filter(is.na(flo_emp_date_end) | flo_emp_date_end == "") %>%
    dplyr::filter(flo_emp_job_type == "TE" | flo_emp_job_type == "SE" | flo_emp_job_type == "IE") %>%
    dplyr::mutate(`GOC Support` = ifelse(flo_emp_job_type == "TE", "Transitional", ifelse(flo_emp_job_type == "SE",
      "Supported", "Independent"
    ))) %>%
    dplyr::left_join(get_members()[, c("id", "first_name", "last_name")], by = c("contact_id" = "id")) %>%
    dplyr::mutate(Member = paste0(last_name, ", ", first_name)) %>%
    dplyr::left_join(get_contacts()[, c("id", "contact2_full_name")], by = c("employer_id" = "id")) %>%
    dplyr::select(`GOC Support`, Member,
      `Job Title` = flo_emp_job_title, Employer = contact2_full_name,
      `Start Date` = flo_emp_date_start,
      `Hourly Pay` = flo_emp_approximate_hourly_pay, `Hours/Week` = flo_emp_approximate_hours_wk
    ) %>%
    dplyr::arrange(desc(`GOC Support`), Member)

  ft <- goc_table(emp, paste0(nrow(emp), " Members employed"), caption = Sys.Date()) %>%
    colformat_num(j = "Hourly Pay", big.mark = ",", prefix = "$")
  ft
}
