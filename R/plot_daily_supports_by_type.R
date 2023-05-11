#' Plot Daily Supports by Type
#'
#' @param date date of plot
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return a plot showing vertical bars with counts of each support type on the day specified
#' @export
#'
#' @examples
#' plot_daily_supports_by_type() # often blank -- supports have not been entered for the current day
#' plot_daily_supports_by_type("2023-03-09")

plot_daily_supports_by_type <- function(date=Sys.Date()-lubridate::days(7)) {
    
    Type=flo_support_date=value=NULL
    
    vars <- list(    
        
        c("full_name", "Name"),
        c("Benefits", "Bene fits"),
        c("Culinary", "Culi nary"),
        c("Education_in_house", "Edu GOC"),
        c("Education_pre_enrollment", "Edu pre"),
        c("Education_post_enrollment", "Edu post"),
        c("Employment_readiness", "Emp pre"),
        c("Employment_IE", "Emp IE"),
        c("Employment_SE", "Emp SE"),
        c("Employment_TE", "Emp TE"),
        c("Development", "Dev"),
        c("Garden", "Gar den"),
        c("Housing", "Hou sing"),
        c("Janitorial", "Jani torial"),
        c("Media", "Me dia"),
        c("Mediation", "Medi ation"),
        c("Office", "Off ice"),
        c("Referral", "Ref erral"),
        c("Social", "Soc ial"),
        c("Spiritual", "Spiri tual"),
        c("Transportation_education", "Van Edu"),
        c("Transportation_employment", "Van Emp"),
        c("Transportation_goc", "Van GOC"),
        c("Transportation_health", "Van Hlth"),
        c("Transportation_wellness", "Van Well"),
        c("Unknown", "Unk"),
        c("Wellness", "Well ness"),
        c("Work_ordered_day", "WOD"),
        c("total", "Total")
    )
    
    translate.support.type <- data.frame(key=sapply(vars, function(x) x[1]), value=sapply(vars, function(x) x[2]))
    
    supports <- get_supports(with.members=T) %>%
        dplyr::filter(flo_support_date == date) %>%
        dplyr::inner_join(get_contacts(), by=c("sa_contacts_2_id"="id")) %>%
        dplyr::inner_join(translate.support.type, by=c("flo_support_type"="key")) %>%
        dplyr::mutate(Type = gsub(" ","\n", value))

    
    ggplot2::ggplot(supports, ggplot2::aes(Type, fill = Type)) +
        ggplot2::ggtitle(paste0("Daily Supports by Type for ", format(lubridate::as_date(date), "%B %d, %Y"))) +
        ggplot2::ylab("Count") +
        ggplot2::xlab("Support Type") +
        ggplot2::geom_bar() +
        ggplot2::theme_linedraw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 32)) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, family="serif", face="italic")) +
        ggplot2::theme(axis.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = 11, face = "bold")) +
        ggplot2::theme(axis.text  = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = 11))
}
