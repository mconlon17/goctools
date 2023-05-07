#' Get goal records from Flourish
#'
#' @param include.deleted If TRUE, include deleted records
#'
#' @return a data.fram of goal records
#' @export
#'
#' @examples
#' get.goals()
get.goals <- function(include.deleted = FALSE) {
    get.table("sa_goals", include.deleted)
}