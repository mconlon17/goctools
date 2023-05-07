#' Get attendance data from Flourish
#'
#' @param include.deleted if TRUE, include deleted attendance records, default is FALSE
#' @param days Number of days from today to include.  Default is Infinite, include all days.  A common value is 90, include all attendance records for the previous 90 days.  Members with attendance in the past 90 days are defined as active members.
#'
#' @return data.frame of attendance records
#' @export
#'
#' @examples
#' nrow(get.attendance()) # number of non-deleted attendance records all time
#' get.attendance() # get all non-deleted attendance records as a data.frame
#' get.attendance(days=90) # attendance over the previous 90 days from today.
get.attendance <- function(include.deleted = FALSE, days=Inf) {
    if (days == Inf) {
        get.table("sa_flourish_daily_attendance", include.deleted)
    } else {
        where <- paste0(" WHERE da_date_of_attendance >= DATE_SUB(NOW(), INTERVAL ",days," DAY);")
        get.table("sa_flourish_daily_attendance", include.deleted, where=where)
    }
}