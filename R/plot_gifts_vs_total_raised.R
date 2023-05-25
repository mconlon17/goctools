#' Plot Number of Gifts vs Total Raised for each campaign
#'
#' Note: small campaigns are excluded
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr n_distinct
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return dot plot of gifts vs raised, labeled by campaign
#' @export
#'
#' @examples
#' plot_gifts_vs_total_raised()
plot_gifts_vs_total_raised <- function() {
  flo_gift_campaign <- flo_gift_amount <- Raised <- id <- Gifts <- NULL

  gifts <- get_gifts() %>%
    dplyr::group_by(flo_gift_campaign) %>%
    dplyr::summarise(Raised = sum(flo_gift_amount), Gifts = dplyr::n_distinct(id)) %>%
    dplyr::filter(Raised > 1000 & Gifts > 5)

  goc_plot(
    ggplot2::ggplot(gifts, ggplot2::aes(Gifts, Raised, label = flo_gift_campaign)) +
      ggplot2::geom_text() +
      ggplot2::ggtitle("Number of Gifts vs Total Raised",
        subtitle = "Small campaigns (under $1000 and number of gifts less than 5) excluded"
      ) +
      ggplot2::ylab("Total Raised") +
      ggplot2::xlab("Number of Gifts")
  )
}
