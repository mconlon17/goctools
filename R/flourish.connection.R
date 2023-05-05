#' Establish a database connection to Flourish
#'
#' @return a database connection
#' 
#' @export
flourish.connection <- function() {
  dbConnect(RMySQL::MySQL(),
    dbname = key_get("Flourish GOC production dbname"),
    host = key_get("Flourish GOC production host"),
    port = 3306,
    user = key_get("Flourish GOC production user"),
    password = key_get("Flourish GOC production password")
  )
}