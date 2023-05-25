#' Get progress notes from Flourish
#'
#' @param include.deleted if TRUE, include deleted progress notes
#'
#' @return a data frame with progress.notes
#' @export
#'
#' @examples
#' get_progress_notes() # get non-deleted progress notes
#' get_progress_notes(include.deleted = TRUE) # get all progress notes, including deleted
get_progress_notes <- function(include.deleted = FALSE) {
  get_table("flo_progress_notes", include.deleted)
}
