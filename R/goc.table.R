#' Prettify display of a data.frame
#' Use `set_flextable_defaults` to control look and feel
#'
#' @param tab the data.frame to be prettified
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
goc.table <- function(tab,caption="") {
    flextable(tab) %>% 
        autofit() %>% 
        colformat_num(big.mark="") %>%
        set_caption(as_paragraph(as_chunk(caption,props=fp_text_default(italic=T))))
}