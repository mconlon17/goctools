#' Get member birthdays for a date
#' 
#' Only active members are included.  Deceased members are not included.  On leave members are included.
#'
#' @param date Members with birthdays on this day will be found
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate year
#'
#' @return Return a text string of first names of members whose birthday falls on the date.  If none, return the string "No birthdays today"
#' @export
#'
#' @examples
#' get_birthdays() # get birthdays for today
#' get_birthdays("2022-05-01")

get_birthdays <- function(date=Sys.Date())  {
    
    id=c2_groups=birthday=birthdate=first_name=NULL
    
    date <- lubridate::ymd(paste(lubridate::year(Sys.Date()), lubridate::month(date), 
                                 lubridate::day(date), sep = "-"), quiet=T)
    
    members <- get_members(active.only=T)
    
    contacts <- get_contacts() %>%
        dplyr::select(id, c2_groups) %>%
        dplyr::filter(!grepl("Deceased", c2_groups))
    
    tab <- members %>%
        dplyr::inner_join(contacts, by = c("contact_id_c" = "id")) %>%
        dplyr::mutate(birthday = lubridate::ymd(paste(lubridate::year(Sys.Date()), lubridate::month(birthdate), 
                                           lubridate::day(birthdate), sep = "-"), quiet=T)) %>%
        dplyr::select(first_name, birthday) %>%
        dplyr::filter(birthday == date)
    
    if (nrow(tab) == 0) { "No birthdays today"} else { paste(tab$first_name, collapse=", ") }
}