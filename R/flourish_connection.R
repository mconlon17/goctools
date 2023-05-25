#' Establish a database connection to Flourish
#'
#' @return a database connection
#'
#' @export
#'
#' @examples
#' con <- flourish_connection()
#' RMySQL::dbDisconnect(con)
flourish_connection <- function() {
  RMySQL::dbConnect(RMySQL::MySQL(),
    dbname   = keyring::key_get("Flourish GOC production dbname"),
    host     = keyring::key_get("Flourish GOC production host"),
    port     = 3306,
    user     = keyring::key_get("Flourish GOC production user"),
    password = keyring::key_get("Flourish GOC production password")
  )
}
