#' Get employment data from Flourish
#'
#' @param include.deleted if TRUE, include deleted employment records
#'
#' @return a data.frame of employment records
#' @export
#'
#' @examples
#' emp <- get.employment()
#' goc.table(emp, "Employment", paste0(nrow(emp), " records as of ", Sys.Date()))

get.employment <- function(include.deleted = FALSE) {
    get.table("sa_employment", include.deleted)
}