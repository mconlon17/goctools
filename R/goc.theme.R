#' GOC theme for flextables
#' 
#' Used in `goc.table` to produce attractive, standard tables
#'
#' @param x The table to get styles
#' @importFrom dplyr %>%
#' @importFrom officer fp_border
#' @importFrom flextable border_outer
#' @importFrom flextable border_inner_h
#' @importFrom flextable border_inner_v
#' @importFrom flextable font
#' @importFrom flextable padding
#' @importFrom flextable bold
#' @importFrom flextable bg
#' @importFrom flextable fontsize
#' @importFrom flextable italic
#' @importFrom flextable align_text_col
#' @importFrom flextable align_nottext_col
#' @importFrom flextable fix_border_issues
#'
#' @return x the table that was entered, with styling
#' @export
#'
#' @examples
#' goc.theme(flextable::flextable(data.frame(x=seq(1,5),y=runif(5))))
#' 
goc.theme <- function(x) 
{
    if (!inherits(x, "flextable")) {
        stop(sprintf("Function `%s` supports only flextable objects.", 
                     "theme_signin()"))
    }
    std_border = officer::fp_border(width = 1)
    x <- flextable::border_outer(x, part="all", border = std_border )
    x <- flextable::border_inner_h(x, border = std_border, part="all")
    x <- flextable::border_inner_v(x, border = std_border, part="all")
    x <- flextable::padding(x, part="all")
    x <- flextable::font(x, font="Times", part = "all")
    
    x <- flextable::bold(x, bold = TRUE, part = "header")
    x <- flextable::fontsize(x, size = 11, part = "header")
    x <- flextable::bg(x, bg = "#EFEFEF", part = "header")
    
    x <- flextable::bold(x, bold = FALSE, part = "body")
    x <- flextable::fontsize(x, size = 10, part="body")
    x <- flextable::bg(x, bg = "#FFFFFF", part = "body")
    
    x <- flextable::bold(x, bold = FALSE, part = "footer")
    x <- flextable::fontsize(x, size = 11, part = "footer")
    x <- flextable::bg(x, bg = "#FFFFFF", part = "footer")
    
    x <- flextable::align_text_col(x, align = "left", header = TRUE)
    x <- flextable::align_nottext_col(x, align = "right", header = TRUE)
    flextable::fix_border_issues(x)
}