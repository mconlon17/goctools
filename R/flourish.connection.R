#' Establish a database connection to Flourish
#'
#' @return a database connection
#' 
#' @examples
#' con <- flourish.connection()
#' RMySQL::dbDisconnect(con)
#' 
#' 
#' @export
flourish.connection <- function() {
  RMySQL::dbConnect(RMySQL::MySQL(),
    dbname   = keyring::key_get("Flourish GOC production dbname"),
    host     = keyring::key_get("Flourish GOC production host"),
    port     = 3306,
    user     = keyring::key_get("Flourish GOC production user"),
    password = keyring::key_get("Flourish GOC production password")
  )
}