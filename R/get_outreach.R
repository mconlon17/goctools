#' Get outreach records from Flourish
#'
#' @param include.deleted If TRUE, included deleted records
#'
#' @return a data.frame of outreach records
#' @export
#'
#' @examples
#' get_outreach()
get_outreach <- function(include.deleted = FALSE) {
    get_table("sa_flourish_reach_out", include.deleted)
}