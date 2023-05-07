#' Get outreach records from Flourish
#'
#' @param include.deleted If TRUE, included deleted records
#'
#' @return a data.frame of outreach records
#' @export
#'
#' @examples
#' get.outreach()
get.outreach <- function(include.deleted = FALSE) {
    get.table("sa_flourish_reach_out", include.deleted)
}