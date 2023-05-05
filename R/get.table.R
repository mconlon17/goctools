#' Get the contents of a Flourish table
#'
#' @param table.name Name of a table in Flourish 
#' @param include.deleted If True, return deleted records as well as well as non-deleted
#' @param where A SQL where clause to be inserted into the database query.  Include the word WHERE
#'
#' @returns A data.frame containing the requested table
#' @seealso [get.table.names()] to get a list of table names in a Flourish database
#' @examples
#' get.table("sa_employment") # return the employment table as a data.frame
#' 
#' @export
get.table <- function(table.name, include.deleted = FALSE, where="") {
    
    con <- flourish.connection()
    
    suppressWarnings({
        
        table <- dbGetQuery(con, paste0("SELECT * FROM ", table.name, " ", where))
        
    })
    
    if (!include.deleted) {
        table <- table %>% filter(!deleted)
    }
    
    dbDisconnect(con)
    table
}