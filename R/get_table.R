#' Return the contents of a Flourish table as a data.frame
#'
#' @param table.name Name of a table in Flourish
#' @param include.deleted If True, return deleted records as well as well as non-deleted
#' @param where A SQL where clause to be inserted into the database query.  Include the word WHERE
#'
#' @returns A data.frame containing the requested table
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @seealso [get_table_names()] to get a list of table names in a Flourish database
#' @examples
#' get_table("sa_employment") # return the employment table as a data.frame
#'
#' @export
get_table <- function(table.name, include.deleted = FALSE, where = "") {
  deleted <- NULL

  con <- flourish_connection()

  suppressWarnings({
    table <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table.name, " ", where))
  })

  if (!include.deleted) {
    table <- table %>% dplyr::filter(!deleted)
  }

  RMySQL::dbDisconnect(con)
  table
}
