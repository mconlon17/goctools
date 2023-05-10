#' Plot total raised all time by payment method
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 rel
#' @importFrom stats reorder
#' @return a plot showing vertical bars for each payment method
#' @export
#'
#' @examples
#' plot_total_raised_by_payment_method()
plot_total_raised_by_payment_method <- function () {
    
    x=flo_gift_payment_method=flo_gift_amount=NULL
    
    ggplot2::ggplot(get_gifts(), ggplot2::aes(x=stats::reorder(flo_gift_payment_method, 
                                flo_gift_amount, function(x) sum(x)))) +
        ggplot2::geom_bar(ggplot2::aes(weight=flo_gift_amount), fill="green") + 
        ggplot2::ggtitle("Total Raised by Payment Method") +
        ggplot2::scale_y_continuous(name="Total Dollars Raised", breaks=seq(0, 125000, by=25000), 
                                    labels=c("$0", "$25,000", "$50,000", "$75,000", "$100,000", "$125,000")) +
        ggplot2::xlab("Payment Method") +
        ggplot2::theme_linedraw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(2.5), face = "bold")) +
        ggplot2::theme(axis.title = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(1.1), face = "bold")) +
        ggplot2::theme(axis.text  = ggplot2::element_text(hjust = 0.5, family = "sans", 
                                                          size = ggplot2::rel(0.9)))
}