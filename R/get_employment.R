#' Get employment data from Flourish
#'
#' @param include.deleted if TRUE, include deleted employment records
#'
#' @return a data.frame of employment records
#' @export
#'
#' @examples
#' emp <- get_employment()
#' goc_table(emp, "Employment", paste0(nrow(emp), " records as of ", Sys.Date()))

get_employment <- function(include.deleted = FALSE) {
    get_table("sa_employment", include.deleted)
}