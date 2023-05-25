#' List New Members
#'
#' @param date The date of the report
#' @param ndays The number of days prior to the report date that a member will be considered new.
#'
#' @return a flextable with the names of the new members and their membership dates
#' @export
#'
#' @examples
#' list_new_members()
#' list_new_members("2023-05-22", ndays = 30)
list_new_members <- function(date = Sys.Date(), ndays = 10) {
  date_entered <- Name <- last_name <- first_name <- `Date Entered` <- NULL

  date <- lubridate::as_date(date)

  members <- get_members() %>%
    dplyr::filter(date - lubridate::as_date(date_entered) <= ndays) %>%
    dplyr::mutate(Name = paste0(last_name, ", ", first_name), date_entered = lubridate::as_date(date_entered)) %>%
    dplyr::arrange(dplyr::desc(date_entered), Name) %>%
    dplyr::select(Name, `Date Entered` = date_entered)

  ft <- goc_table(members, "New Members", caption = paste0("Since ", date - days(ndays)))
  ft
}
