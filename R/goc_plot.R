#' GOC Plot
#'
#' @param p a plot to be goc-styled
#'
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @return a plot that is goc-styled
#' @export
#'
#' @examples
#' d <- data.frame(x = 1:5, y = 4:8)
#' goc_plot(ggplot2::ggplot(d, ggplot2::aes(x, y)) +
#'   ggplot2::geom_line())
goc_plot <- function(p) {
  p + ggplot2::theme_linedraw() +
    ggplot2::theme(plot.title = ggplot2::element_text(
      hjust = 0.5, family = "sans",
      size = 32
    )) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(
      hjust = 0.5, family = "serif",
      size = 11, face = "italic"
    )) +
    ggplot2::theme(axis.title = ggplot2::element_text(
      hjust = 0.5, family = "sans",
      size = 11, face = "bold"
    )) +
    ggplot2::theme(axis.text = ggplot2::element_text(
      hjust = 0.5, family = "sans",
      size = 11
    ))
}
