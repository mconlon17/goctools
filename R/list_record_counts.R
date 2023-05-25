#' List record counts
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom RMySQL dbDisconnect
#' @importFrom stringr str_to_title
#'
#' @return a flextable of record counts for each table
#' @export
#'
#' @examples
#' list_record_counts()
list_record_counts <- function() {
  Deleted <- `Last Entered` <- `Last Modified` <- `Non-Deleted Records` <- Table <- NULL
  `Total Records` <- count <- last_entered <- last_modified <- NULL

  dict <- dict_table_names()
  tables <- dict$key
  table.names <- dict$value

  con <- flourish_connection()

  deleted <- data.frame()

  for (table_name in tables) {
    stmt <- paste("SELECT COUNT(*) AS count, MAX(date_entered) AS last_entered,
                MAX(date_modified) AS last_modified FROM", table_name, "WHERE deleted = 1")
    result <- DBI::dbGetQuery(con, stmt)
    deleted <- deleted %>% dplyr::bind_rows(data.frame(result))
  }

  deleted <- deleted %>%
    dplyr::rename(Deleted = count)

  record.tracking <- data.frame()

  for (table_name in tables) {
    stmt <- paste("SELECT COUNT(*) AS count, MAX(date_entered) AS last_entered,
                MAX(date_modified) AS last_modified FROM", table_name)
    result <- DBI::dbGetQuery(con, stmt)
    record.tracking <- record.tracking %>% dplyr::bind_rows(data.frame(result))
  }

  x <- RMySQL::dbDisconnect(con)

  record.tracking <- record.tracking %>%
    dplyr::rename(`Total Records` = count, `Last Entered` = last_entered, `Last Modified` = last_modified) %>%
    dplyr::bind_cols(data.frame(Table = stringr::str_to_title(table.names))) %>%
    dplyr::bind_cols(data.frame(Deleted = deleted$Deleted)) %>%
    dplyr::mutate(`Non-Deleted Records` = `Total Records` - Deleted) %>%
    dplyr::select(Table, `Non-Deleted Records`, `Deleted Records` = Deleted, `Total Records`, `Last Entered`, `Last Modified`) %>%
    dplyr::arrange(Table)

  ft <- goc_table(record.tracking, paste0("Flourish Record Counts for ", Sys.Date()))
  ft
}
