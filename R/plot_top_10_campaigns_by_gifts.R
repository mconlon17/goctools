#' Plot Top 10 Campaigns by Gifts
#'
#' @return a donut plot with share as fraction of gifts in each of top 10 campaigns
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr slice_max
#' @importFrom dplyr rename
#' @importFrom utils head
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @export
#'
#' @examples
#' plot_top_10_campaigns_by_gifts()
   
plot_top_10_campaigns_by_gifts <- function() {
    
    Campaign=flo_gift_campaign=n=ymax=ymin=NULL
    
    gifts <- get_gifts() %>%
        dplyr::group_by(flo_gift_campaign) %>%
        dplyr::tally() %>%
        dplyr::slice_max(n, n=10) %>%
        dplyr::rename(Campaign = flo_gift_campaign)
    
    dat <- gifts
    dat$fraction = dat$n / sum(dat$n)
    dat = dat[order(dat$fraction), ]
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, utils::head(dat$ymax, n=-1))
    
    ggplot2::ggplot(dat, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = stats::reorder(Campaign,1/n))) +
        ggplot2::geom_rect(colour="grey30") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::theme_bw() +
        ggplot2::xlim(c(0,4)) +
        ggplot2::scale_fill_discrete(name = "Campaign") +
        ggplot2::theme(axis.ticks=element_blank()) +
        ggplot2::theme(axis.text=element_blank()) +
        ggplot2::theme(panel.grid=element_blank()) +
        ggplot2::ggtitle("Top 10 Campaigns by Number of Gifts") + 
        ggplot2::xlab("") + 
        ggplot2::ylab("") + 
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, family = "sans", size = 32))
}
