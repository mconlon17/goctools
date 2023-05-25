#' Get goal records from Flourish
#'
#' @param include.deleted If TRUE, include deleted records
#'
#' @return a data.fram of goal records
#' @export
#'
#' @examples
#' get_goals()
get_goals <- function(include.deleted = FALSE) {
  get_table("sa_goals", include.deleted)
}
