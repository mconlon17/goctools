#' Tracking Data by Member and Staff
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#'
#' @return a flextable showing active members, their assigned staff, and tracking data consisting of number of visits in the
#' past 90 days and date of most recent visit, supports and most recent support, open goals and date of most recent goal, and
#' progress notes in the last 90 days and date of most recent progress note
#' @export
#'
#' @examples
#' list_tracking_by_member_and_staff()
list_tracking_by_member_and_staff <- function() {
  `Member\nName` <- Notes <- OpenGoals <- RecentAttend <- RecentGoal <- RecentNote <- NULL
  RecentSupport <- `Staff\nName` <- SupportID <- Supports <- Visits <- contact_id_c <- NULL
  contacts <- da_date_of_attendance <- first_name <- first_name.x <- first_name.y <- NULL
  flo_date_of_note <- flo_goal_creation_date <- flo_support_date <- NULL
  goc_contact_id <- id <- last_name <- name <- staff_name <- flo_goal_end_date <- NULL

  goals <- get_goals()[, c("contact_id", "name", "flo_goal_creation_date", "flo_goal_end_date")] %>%
    dplyr::filter(is.na(flo_goal_end_date))

  attendance <- get_attendance(days = 90)[, c("contact_2_id", "da_date_of_attendance")] %>%
    dplyr::mutate(da_date_of_attendance = as_date(da_date_of_attendance))

  support.rel <- get_table("flo_supports_contacts_c") %>%
    dplyr::select("flo_supports_contactsflo_supports_ida", "flo_supports_contactscontacts_idb")

  supports <- get_supports()[, c("id", "name", "flo_support_date")] %>%
    dplyr::mutate(flo_support_date = as_date(flo_support_date)) %>%
    dplyr::mutate(SupportID = id) %>%
    dplyr::filter(flo_support_date >= (Sys.Date() - days(90))) %>%
    dplyr::inner_join(support.rel, by = c("id" = "flo_supports_contactsflo_supports_ida")) %>%
    dplyr::ungroup()

  pn <- get_progress_notes()[, c("contact_id", "flo_date_of_note")] %>%
    dplyr::mutate(flo_date_of_note = as_date(flo_date_of_note)) %>%
    dplyr::filter(flo_date_of_note >= (Sys.Date() - days(90)))

  members <- get_members(active.only = T) %>%
    dplyr::select(id, contact_id_c, goc_contact_id, first_name, last_name) %>%
    # Add the staff name

    dplyr::left_join(get_contacts()[, c("id", "first_name")], contacts, by = c("goc_contact_id" = "id")) %>%
    dplyr::rename(staff_name = first_name.y, first_name = first_name.x) %>%
    # Add Open Goals

    dplyr::left_join(goals, by = c("id" = "contact_id")) %>%
    dplyr::group_by(id, contact_id_c, first_name, last_name, staff_name) %>%
    dplyr::summarise(OpenGoals = n_distinct(name, na.rm = T), RecentGoal = max(as_date(flo_goal_creation_date), na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(RecentGoal = as_date(ifelse(RecentGoal == -Inf, NA, RecentGoal))) %>%
    # Add Attendance

    dplyr::left_join(attendance, by = c("contact_id_c" = "contact_2_id")) %>%
    dplyr::group_by(id, first_name, last_name, staff_name, OpenGoals, RecentGoal) %>%
    dplyr::summarise(Visits = n_distinct(da_date_of_attendance, na.rm = T), RecentAttend = max(da_date_of_attendance, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # Add Supports

    dplyr::left_join(supports, by = c("id" = "flo_supports_contactscontacts_idb")) %>%
    dplyr::group_by(id, first_name, last_name, staff_name, OpenGoals, RecentGoal, Visits, RecentAttend) %>%
    dplyr::summarise(Supports = n_distinct(SupportID, na.rm = T), RecentSupport = max(flo_support_date, na.rm = T)) %>%
    dplyr::mutate(RecentSupport = as_date(ifelse(RecentSupport == -Inf, NA, RecentSupport))) %>%
    dplyr::ungroup() %>%
    # Add Progress Supports

    dplyr::left_join(pn, by = c("id" = "contact_id")) %>%
    dplyr::group_by(id, first_name, last_name, staff_name, OpenGoals, RecentGoal, Visits, RecentAttend, Supports, RecentSupport) %>%
    dplyr::summarise(Notes = n_distinct(flo_date_of_note, na.rm = T), RecentNote = max(flo_date_of_note, na.rm = T)) %>%
    dplyr::mutate(RecentNote = as_date(ifelse(RecentNote == -Inf, NA, RecentNote))) %>%
    dplyr::ungroup() %>%
    # Cosmetics

    dplyr::mutate(`Member\nName` = paste0(last_name, ", ", first_name)) %>%
    dplyr::select(
      `Staff\nName` = staff_name, `Member\nName`,
      `Visits\nin\nlast\n90 Days` = Visits, `Most\nRecent\nVisit` = RecentAttend,
      `Supports\nin\nlast\n90 Days` = Supports, `Most\nRecent\nSupport` = RecentSupport,
      `Open\nGoals` = OpenGoals, `Most\nRecent\nGoal` = RecentGoal,
      `Progress\nNotes in\nlast 90 Days` = Notes, `Most\nRecent\nProgress\nNote` = RecentNote,
    ) %>%
    dplyr::arrange(`Staff\nName`, `Member\nName`)

  ft <- goc_table(members,
    heading = "Tracking By Members and Staff",
    caption = paste0(nrow(members), " Active Members ", nrow(members[!is.na(members$`Staff\nName`), ]), " Assigned to Staff")
  )
  ft
}
