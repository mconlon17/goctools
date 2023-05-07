#' Get users table from Flourish
#'
#' @param include.deleted if TRUE, include deleted records.  Default is FALSE
#'
#' @return a data.frame corresponding to the the SQL users table in Flourish
#' @export
#'
#' @examples
#' get.users() # return all non-deleted records
#' get.users(include.deleted=TRUE) # return all records, including deleted
#' get.users()[,c("id","date_entered")] # return named columns for non-deleted records
#' colnames(get.users()) # return the column names of the users table
get.users <- function(include.deleted = FALSE) {
    get.table("users", include.deleted)
}