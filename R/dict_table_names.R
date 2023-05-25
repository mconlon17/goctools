#' Dictionary of table names
#'
#' Provides a translation between the external name of the table, known as the value: contacts, members, etc, and the internal name of the table, known as the key.  In this way, a dict_ function performs like a python dict object.
#'
#' @return a list with two elements -- `key`, a vector of keys, and `value`, a vector of values
#' @export
#'
#' @examples
#' dict_table_names()$key[2] # key value for contacts table
#' dict_table_names()$key[which(dict_table_names()$value == "contacts")]
dict_table_names <- function() {
  d <- c(
    "sa_flourish_daily_attendance", "attendance",
    "sa_contacts_2", "contacts",
    "contacts", "members",
    "sa_flourish_reach_out", "outreach",
    "sa_goals", "goals",
    "flo_supports", "supports",
    "flo_progress_notes", "progress_notes",
    "flo_gifts", "gifts",
    "sa_employment", "employment",
    "users", "users"
  )
  list(key = d[c(TRUE, FALSE)], value = d[c(FALSE, TRUE)])
}
