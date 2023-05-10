#' Get a vector containing the names of the tables in a database
#'
#' @return a vector of table names
#' 
#' @export
#' 
#' @examples
#' names <- get_table_names()

get_table_names <- function() {
    con <- flourish_connection()
    x <- RMySQL::dbListTables(con)
    RMySQL::dbDisconnect(con)
    x
}