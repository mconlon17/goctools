#' dict_city_county
#'
#' @return dictionary of counties for cities.  key is city name, value is county name
#' @export
#'
#' @examples
#' dict_city_county()

dict_city_county <- function() {
    
    cities <- c(
        "Alachua", "Archer", "Bell", "Casselberry", "Cross Creek", "Earleton", "Fort_McCoy", "Fort_White", "Gainesville",
        "Hawthorne", "High Springs", "Inglis", "Interlachen", "Keystone Heights", "Lake Butler", "Lake_City",
        "Melrose", "Micanopy", "Newberry", "Ocala", "Palatka", "Palm Harbor", "Plantation", "Silver Springs", "Starke", "Trenton",
        "Unknown", "Waldo", "Williston"
    )
    counties <- c(
        "Alachua", "Alachua", "Gilchrist", "Seminole", "Alachua", "Alachua", "Marion", "Columbia", "Alachua",
        "Alachua", "Alachua", "Levy", "Putnam", "Clay", "Union", "Columbia",
        "Clay", "Alachua", "Alachua", "Marion", "Putnam", "Pinellas", "Broward", "Marion", "Bradford", "Gilchrist",
        "Unknown", "Alachua", "Levy"
    )
    data.frame(key = cities, value = counties)
}
