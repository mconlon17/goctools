#' Check Member Info
#'
#' Produce a table a member information
#'
#' @param active.only If TRUE, only active members are included
#' @param check What to check.  Choices are "contacts", "emergency", "benefits", "demographics",
#' "housing", "education", "employment", medical", "referral", "admin", and "attendance"
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#'
#' @return a flextable of member information
#' @export
#'
#' @examples
#' check_members()
#' check_members("demographics")
#' check_members("housing", FALSE)
check_members <- function(check = "contacts", active.only = T) {
  Name <- first_name <- last_name <- flo_primary_phone <- primary_address_street <- primary_address_city <- primary_address_state <- primary_address_postalcode <- NULL
  flo_emergency_con_phone_c <- emergency_address_street <- emergency_address_city <- emergency_address_state <- emergency_address_postalcode <- NULL
  flo_ssi <- flo_ssdi <- flo_food_stamps <- flo_housing_voucher <- NULL
  flo_deceased_c <- birthdate <- flo_gender_at_birth_c <- flo_gender_identity_c <- flo_preferred_pronouns_c <- NULL
  flo_race_mult <- flo_ethnicity_hispanic_c <- flo_type_young_adults_c <- flo_type_senior_citizen_c <- NULL
  flo_considered_veteran_c <- flo_considered_lgbt_c <- flo_marital_status_c <- flo_primary_language_c <- flo_secondary_language_c <- NULL
  flo_housing_status_c <- flo_housing_comments <- NULL
  flo_highest_grade_completed <- flo_degrees_certificates <- flo_education_comments <- NULL
  flo_ever_employed <- flo_last_employment <- flo_employment_comments <- flo_annual_income <- NULL
  flo_diagnoses_mult <- flo_diagnosis_comments <- flo_accessibility_restrictions <- flo_allergies <- NULL
  referral_type <- `Referring Individual` <- `Referring Agency` <- contact2_full_name <- NULL
  flo_membership_status_c <- flo_transportation_mode_c <- flo_unit_chosen_by_member_c <- NULL
  flo_application_date <- flo_emergency_release <- flo_information_release <- flo_media_release <- NULL
  last_attended <- avg_time_at_ch <- NULL

  members <- get_members(active.only = T) %>%
    dplyr::mutate(Name = paste0(last_name, ", ", first_name)) %>%
    dplyr::arrange(Name)

  if (check == "contacts") {
    tab <- members %>%
      dplyr::select(
        Name, flo_primary_phone, primary_address_street, primary_address_city, primary_address_state,
        primary_address_postalcode
      )
    ft <- goc_table(tab, "Member Contact List")
  }

  if (check == "emergency") {
    tab <- members %>%
      dplyr::select(
        Name, flo_emergency_con_phone_c, emergency_address_street, emergency_address_city, emergency_address_state,
        emergency_address_postalcode
      )

    ft <- goc_table(tab, "Member Emergency Contacts")
  }

  if (check == "benefits") {
    tab <- members %>%
      dplyr::select(Name, flo_ssi, flo_ssdi, flo_food_stamps, flo_housing_voucher)

    ft <- goc_table(tab, "Member Benefit Information")
  }

  if (check == "demographics") {
    tab <- members %>%
      dplyr::select(
        Name, flo_deceased_c, birthdate, flo_gender_at_birth_c, flo_gender_identity_c, flo_preferred_pronouns_c,
        flo_race_mult, flo_ethnicity_hispanic_c, flo_type_young_adults_c, flo_type_senior_citizen_c,
        flo_considered_veteran_c, flo_considered_lgbt_c, flo_marital_status_c, flo_primary_language_c,
        flo_secondary_language_c
      )

    ft <- goc_table(tab, "Member Demographics")
    ft
  }

  if (check == "housing") {
    tab <- members %>%
      dplyr::select(Name, flo_housing_status_c, flo_housing_comments)

    ft <- goc_table(tab, "Member Housing")
  }

  if (check == "education") {
    tab <- members %>%
      dplyr::select(Name, flo_highest_grade_completed, flo_degrees_certificates, flo_education_comments)

    ft <- goc_table(tab, "Member Education")
  }

  if (check == "employment") {
    tab <- members %>%
      dplyr::select(Name, flo_ever_employed, flo_last_employment, flo_employment_comments, flo_annual_income)

    ft <- goc_table(tab, "Member Employment")
  }

  if (check == "medical") {
    tab <- members %>%
      dplyr::select(
        Name, flo_diagnoses_mult, flo_diagnosis_comments, flo_accessibility_restrictions,
        flo_allergies
      )

    ft <- goc_table(tab, "Member Medical")
  }

  if (check == "referral") {
    tab <- members %>%
      dplyr::left_join(get_contacts()[, c("id", "contact2_full_name")], by = c("referrer_id_c" = "id")) %>%
      dplyr::rename(`Referring Individual` = contact2_full_name) %>%
      dplyr::left_join(get_contacts()[, c("id", "contact2_full_name")], by = c("referral_source_id_c" = "id")) %>%
      dplyr::select(Name, referral_type, `Referring Individual`, `Referring Agency` = contact2_full_name)

    ft <- goc_table(tab, "Member Referral")
  }

  if (check == "admin") {
    tab <- members %>%
      dplyr::select(
        Name, flo_membership_status_c, flo_transportation_mode_c, flo_unit_chosen_by_member_c,
        flo_application_date, flo_emergency_release, flo_information_release, flo_media_release
      )

    ft <- goc_table(tab, "Member Admin")
  }

  if (check == "attendance") {
    tab <- members %>%
      dplyr::select(Name, last_attended, avg_time_at_ch)

    ft <- goc_table(tab, "Member Attendance", "Note: This table is awaiting overnight computations")
  }
  ft
}
