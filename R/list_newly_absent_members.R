#' List Newly Absent Members
#'
#' @param date Date to be checked for newly absent members
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#'
#' @return a flextable showing members absent more than 90 days and less than 180 days
#' @export
#'
#' @examples
#' list_newly_absent_members()
list_newly_absent_members <- function(date = Sys.Date()) {
  Name <- last_name <- c2_groups <- first_name <- id <- Since <- da_date_of_attendance <- `Last Visit` <- NULL

  date <- lubridate::as_date(date)

  contacts <- get_contacts() %>%
    filter(grepl("Member", c2_groups)) %>%
    mutate(Name = paste(last_name, first_name, sep = ", ")) %>%
    select(Name, id)

  absent <- get_attendance() %>%
    inner_join(contacts, by = c("contact_2_id" = "id")) %>%
    mutate(Since = date - lubridate::as_date(da_date_of_attendance)) %>%
    group_by(Name) %>%
    summarise(`Last Visit` = min(Since)) %>%
    filter(`Last Visit` > 90 & `Last Visit` < 180) %>%
    arrange(`Last Visit`) %>%
    select(Name, `Last Visit`)

  ft <- goc_table(absent, paste0("Members Absent over 90 days as of ", date))
  ft
}
