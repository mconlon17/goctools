#' Get a vector containing the names of the tables in a database
#'
#' @return a vector of table names
#' 
#' @export
get.table.names <- function() {
    con <- flourish.connection()
    x <- RMySQL::dbListTables(con)
    RMySQL::dbDisconnect(con)
    x
}