#' Get a vector containing the names of the tables in a database
#'
#' @return a vector of table names
get.table.names <- function() {
    con <- flourish.connection()
    x <- dbListTables(con)
    dbDisconnect(con)
    x
}