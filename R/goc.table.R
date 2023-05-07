#' Prettify display of a data.frame
#'
#' Given a data.frame and an optional caption, display the data.frame as a table suitable for printing, web dislay, or saving. Use `set_flextable_defaults` to control look and feel
#'
#' @param df the data.frame to be prettified
#' @param caption an optional caption to display at the top of the display
#' @importFrom dplyr %>%
#' @importFrom flextable flextable
#' @importFrom flextable autofit
#' @importFrom flextable colformat_num
#' @importFrom flextable set_caption
#' @importFrom flextable as_paragraph
#' @importFrom flextable as_chunk
#' @importFrom flextable fp_text_default
#' @return a flextable
#' @examples
#' goc.table(get.table('users'), 'User accounts on GOC Flourish')
#' @export
goc.table <- function(df,caption="") {
    flextable(df) %>% 
        autofit() %>% 
        colformat_num(big.mark="") %>%
        set_caption(as_paragraph(as_chunk(caption,props=fp_text_default(italic=T))))
}