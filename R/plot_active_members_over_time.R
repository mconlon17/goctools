#' Plot Active Members over Time
#'
#' @param start_date plot runs from start date to today
#' @importFrom lubridate as_date
#' @importFrom lubridate years
#' @importFrom shiny isRunning
#' @importFrom shiny Progress
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return a plot showing active members over time
#' @export
#'
#' @examples
#' plot_active_members_over_time()
plot_active_members_over_time <- function(start_date = Sys.Date() - lubridate::years(1)) {
  
  MostRecent=Since=c2_groups=contact_2_id=da_date_of_attendance=NULL
    
  if (lubridate::as_date(start_date) == as_date(Sys.Date())) {
    start_date <- start_date - days(365)
  }
  
  ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1

  if (shiny::isRunning()) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Getting Members", value = 0)
  }

  contacts <- get_contacts() %>%
    dplyr::filter(grepl("Member", c2_groups))

  if (shiny::isRunning()) {
    progress$set(message = "Getting Attendance", value = 0.1)
  }

  member.attendance <- get_attendance(days = ndays) %>%
    dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>% # Inner join must be a Member with attendance
    dplyr::select(contact_2_id, da_date_of_attendance)

  if (shiny::isRunning()) {
    progress$set(message = "Counting Active Members", value = 0.2)
  }

  dates <- seq(from = lubridate::as_date(start_date), to = Sys.Date() - 7, by = "weeks")
  ndates <- length(dates)

  active.members <- c()
  for (date in dates) {
    n <- nrow(member.attendance %>%
      dplyr::mutate(Since = as_date(date) - as_date(da_date_of_attendance)) %>%
      dplyr::group_by(contact_2_id) %>%
      dplyr::summarise(MostRecent = min(abs(Since))) %>%
      dplyr::filter(MostRecent < 90)) # Active members on the date
    active.members <- c(active.members, n)

    if (shiny::isRunning()) {
      progress$inc(0.8 * (1 / ndates), detail = format(as_date(date), "%Y %b %d"))
    }
  }
  active <- data.frame(dates = lubridate::as_date(dates), active.members = active.members)

  average <- mean(active$active.members)

  ggplot2::ggplot(active, aes(x = dates, y = active.members)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(color = "blue", yintercept = average) +
    ggplot2::ggtitle(paste0("Active Members from ", start_date), 
                     subtitle = paste0("Average Active Members = ", format(average, digits = 3))) +
    ggplot2::scale_x_date(name = "Years", date_labels = '%b %Y', date_breaks = '3 months') +
    ggplot2::scale_y_continuous(name = "Active Members", n.breaks=5) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 32)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, family="serif", face="italic")) +
    ggplot2::theme(axis.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                      size = 11, face = "bold")) +
    ggplot2::theme(axis.text  = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                      size = 11))
}
