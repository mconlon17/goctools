#' Check Employment
#'
#' @param check The employment check to be run, one of member, employer, start, support, pay, or hours

#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @return a flextable showing checks for the value specified
#' @export
#'
#' @examples
#' check_employment()
#' check_employment("hours")
check_employment <- function(check = "member") {
  `Employment ID` <- name <- contact_id <- NULL
  `Hours per Week` <- flo_emp_approximate_hours_wk <- n <- NULL
  `Hourly Pay` <- flo_emp_approximate_hourly_pay <- NULL
  `Support Type` <- flo_emp_job_type <- NULL
  employer_id <- flo_emp_date_start <- NULL

  emp <- get_employment()

  if (check == "member") {
    tab <- emp %>%
      dplyr::filter(contact_id == "") %>%
      dplyr::select(`Employment ID` = name)

    ft <- goc_table(tab, paste0(nrow(tab), " Employment Records with no Member"))
    ft
  }

  if (check == "employer") {
    tab <- emp %>%
      dplyr::filter(employer_id == "") %>%
      dplyr::select(`Employment ID` = name)

    ft <- goc_table(tab, paste0(nrow(tab), " Employment Records with no Employer"))
    ft
  }

  if (check == "start") {
    tab <- emp %>%
      dplyr::filter(flo_emp_date_start == "" | is.na(flo_emp_date_start)) %>%
      dplyr::select(`Employment ID` = name)

    ft <- goc_table(tab, paste0(nrow(tab), " Employment Records with no Start Date"))
    ft
  }

  if (check == "support") {
    tab <- emp %>%
      dplyr::group_by(flo_emp_job_type) %>%
      dplyr::tally() %>%
      dplyr::select(`Support Type` = flo_emp_job_type, n)

    ft <- goc_table(tab, paste0("Employment Support"))
    ft
  }

  if (check == "pay") {
    tab <- emp %>%
      dplyr::group_by(flo_emp_approximate_hourly_pay) %>%
      dplyr::tally() %>%
      dplyr::select(`Hourly Pay` = flo_emp_approximate_hourly_pay, n)

    ft <- goc_table(tab, paste0("Hourly Pay"))
    ft
  }

  if (check == "hours") {
    tab <- emp %>%
      dplyr::group_by(flo_emp_approximate_hours_wk) %>%
      dplyr::tally() %>%
      dplyr::select(`Hours per Week` = flo_emp_approximate_hours_wk, n)

    ft <- goc_table(tab, paste0("Hours Per Week"))
    ft
  }
  ft
}
