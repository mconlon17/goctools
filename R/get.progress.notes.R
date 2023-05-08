#' Get progress notes from Flourish
#'
#' @param include.deleted if TRUE, include deleted progress notes
#'
#' @return a data fram with progress.notes
#' @export
#'
#' @examples
#' get.progress.notes() # get non-deleted progress notes
#' get.progress.notes(include.deleted=TRUE) # get all progress notes, including deleted
get.progress.notes <- function(include.deleted = FALSE) {
    get.table("flo_progress_notes", include.deleted)
}