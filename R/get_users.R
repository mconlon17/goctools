#' Get users table from Flourish
#'
#' @param include.deleted if TRUE, include deleted records.  Default is FALSE
#'
#' @return a data.frame corresponding to the the SQL users table in Flourish
#' @export
#'
#' @examples
#' get_users() # return all non-deleted records
#' get_users(include.deleted = TRUE) # return all records, including deleted
#' get_users()[, c("id", "date_entered")] # return named columns for non-deleted records
#' colnames(get_users()) # return the column names of the users table
get_users <- function(include.deleted = FALSE) {
  get_table("users", include.deleted)
}
