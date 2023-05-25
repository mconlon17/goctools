#' Supported Employment Billing List
#'
#' @param date Date in month for billing list
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom lubridate as_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate %within%
#' @importFrom lubridate %--%
#'
#' @return a flextable containing the data required to prepare grant billing for supported employment
#' @export
#'
#' @examples
#' list_supported_employment_billing()
#' list_supported_employment_billing("2023-04-05")
list_supported_employment_billing <- function(date = Sys.Date()) {
  Benefits <- DischargeDate <- EmploymentPlanUpdate <- LastVisit <- NULL
  MostRecentSupport <- Pre <- RecentAssess <- SES <- contact2_full_name <- NULL
  contact_2_id <- contact_flourish_number <- contact_id <- contact_id_c <- NULL
  da_date_of_attendance <- date_modified <- emp_assessment_date <- first_name <- NULL
  flo_emp_approximate_hourly_pay <- flo_emp_approximate_hours_wk <- NULL
  flo_emp_date_end <- flo_emp_date_start <- flo_emp_job_title <- flo_food_stamps <- NULL
  flo_goal_end_date <- flo_goal_type <- flo_housing_voucher <- flo_ssdi <- flo_ssi <- NULL
  flo_support_date <- flo_support_type <- NULL
  flo_supports_contactsflo_supports_ida <- id <- last_name <- sa_contacts_2_id <- NULL

  date <- lubridate::as_date(date)
  start_date <- lubridate::floor_date(date, "month")
  end_date <- lubridate::ceiling_date(date, "month") - lubridate::days(1)
  ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

  members <- get_members() %>%
    dplyr::mutate(Benefits = ifelse(flo_ssi == "yes" | flo_ssdi == "yes" |
      flo_food_stamps == "yes" | flo_housing_voucher == "yes", "Yes", "No")) %>%
    dplyr::select(id, contact_flourish_number, contact_id_c, last_name, first_name, Benefits)

  attendance <- get_attendance(days = ndays) %>%
    dplyr::mutate(da_date_of_attendance = lubridate::as_date(da_date_of_attendance)) %>%
    dplyr::filter(da_date_of_attendance %within% (start_date %--% end_date)) %>%
    dplyr::select(contact_2_id, da_date_of_attendance)

  members <- members %>%
    dplyr::inner_join(attendance, by = c("contact_id_c" = "contact_2_id")) %>%
    dplyr::group_by(id, contact_flourish_number, contact_id_c, last_name, first_name, Benefits) %>%
    dplyr::summarise(LastVisit = max(da_date_of_attendance)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(last_name, first_name)

  supports <- get_supports() %>%
    dplyr::filter(lubridate::as_date(flo_support_date) %within% (start_date %--% end_date)) %>%
    dplyr::filter(flo_support_type == "Employment_readiness" |
      flo_support_type == "Employment_TE" |
      flo_support_type == "Employment_IE" |
      flo_support_type == "Employment_SE") %>%
    dplyr::mutate(Pre = ifelse(flo_support_type == "Employment_readiness", "Yes", "No")) %>%
    dplyr::mutate(SES = ifelse(flo_support_type != "Employment_readiness", "Yes", "No")) %>%
    dplyr::select(id, sa_contacts_2_id, Pre, SES, flo_support_date)

  support.rel <- get_table("flo_supports_contacts_c") %>%
    dplyr::select("flo_supports_contactsflo_supports_ida", "flo_supports_contactscontacts_idb")

  members <- members %>%
    dplyr::left_join(support.rel, by = c("id" = "flo_supports_contactscontacts_idb")) %>%
    dplyr::left_join(supports, by = c("flo_supports_contactsflo_supports_ida" = "id")) %>%
    dplyr::select(-flo_supports_contactsflo_supports_ida) %>%
    dplyr::group_by(id, contact_flourish_number, contact_id_c, last_name, first_name, LastVisit, Benefits) %>%
    dplyr::summarise(Pre = max(Pre, na.rm = T), SES = max(SES, na.rm = T), MostRecentSupport = max(flo_support_date, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Pre = ifelse(Pre == -Inf | is.na(Pre), "No", Pre)) %>%
    dplyr::mutate(SES = ifelse(SES == -Inf | is.na(SES), "No", SES))

  goals <- get_goals()[, c("contact_id", "flo_goal_type", "flo_goal_end_date", "date_modified")] %>%
    dplyr::filter(flo_goal_type == "Employment") %>%
    dplyr::filter(!is.na(flo_goal_end_date) & lubridate::as_date(flo_goal_end_date) <= end_date) %>%
    dplyr::filter(!is.na(date_modified) & lubridate::as_date(date_modified) <= end_date)

  members <- members %>%
    dplyr::left_join(goals, by = c("id" = "contact_id")) %>%
    dplyr::group_by(id, contact_flourish_number, contact_id_c, last_name, first_name, LastVisit, Benefits, Pre, SES, MostRecentSupport) %>%
    dplyr::summarise(
      DischargeDate = max(lubridate::as_date(flo_goal_end_date), na.rm = T),
      EmploymentPlanUpdate = max(date_modified, na.rm = T)
    ) %>%
    dplyr::mutate(
      DischargeDate = lubridate::as_date(ifelse(DischargeDate == -Inf, NA, DischargeDate)),
      EmploymentPlanUpdate = lubridate::as_date(ifelse(EmploymentPlanUpdate == -Inf, NA, EmploymentPlanUpdate))
    )

  assess <- get_employment()[, c("contact_id", "emp_assessment_date")] %>%
    dplyr::group_by(contact_id) %>%
    dplyr::summarise(RecentAssess = max(emp_assessment_date, na.rm = T)) %>%
    dplyr::ungroup()

  members <- members %>%
    dplyr::left_join(assess, by = c("id" = "contact_id"))

  # Employment records

  members <- members %>%
    dplyr::left_join(get_employment()[, c(
      "contact_id", "flo_emp_date_start",
      "flo_emp_job_title",
      "flo_emp_date_end", "flo_emp_job_title",
      "flo_emp_approximate_hourly_pay",
      "flo_emp_approximate_hours_wk",
      "employer_id"
    )], by = c("id" = "contact_id")) %>%
    dplyr::left_join(get_contacts()[, c("id", "contact2_full_name")], by = c("employer_id" = "id"))

  members <- members %>%
    dplyr::arrange(last_name, first_name) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      `Client's Last Name` = last_name,
      `Client's First Name` = first_name,
      `Client ID` = contact_flourish_number,
      `Most Recent Clubhouse Attendance Date` = LastVisit,
      `Did this Client Receive Pre-Employment Supports this Month? (Yes/No)` = Pre,
      `Is This Client Receiving Benefits?` = Benefits,
      `Did This Client Receive Supported Employment Services this Month? (Yes/No)` = SES,
      `Most Recent Supported Employment Service Date` = MostRecentSupport,
      `Start Date of Most Recent Job` = flo_emp_date_start,
      `End Date of Most Recent Job` = flo_emp_date_end,
      `Supported Employment Initial Assessment Completion Date` = RecentAssess,
      `Employer Name` = contact2_full_name,
      `Supported Employment Initial Service Plan Creation Date` = EmploymentPlanUpdate,
      `Supported Employment Discharge Date` = DischargeDate,
      `Job Title` = flo_emp_job_title,
      `Average # Hours Worked Per Week` = flo_emp_approximate_hours_wk,
      `Hourly Pay` = flo_emp_approximate_hourly_pay,
      -id,
      -contact_id_c
    )

  ft <- goc_table(members, paste0(nrow(members), " Attending Members in ", format(date, "%B, %Y")))
  ft
}
