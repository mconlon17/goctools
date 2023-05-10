#' Prettify display of a data.frame
#'
#' Given a data.frame and an optional caption, display the data.frame as a table suitable for printing, web display, or saving. Use `set_flextable_defaults` to control look and feel
#'
#' @param df the data.frame to be prettified
#' @param heading an optional heading to display at the top of the display
#' @param caption an optional caption to display at the top of the display
#' @importFrom dplyr %>%
#' @importFrom flextable flextable
#' @importFrom flextable autofit
#' @importFrom flextable colformat_num
#' @importFrom flextable set_caption
#' @importFrom flextable as_paragraph
#' @importFrom flextable as_chunk
#' @importFrom flextable fp_text_default
#' @return a prettified flextable
#' @export
#' 
#' @examples
#' goc_table(get_table("users"), "User accounts on GOC Flourish")

goc_table <- function(df, heading="", caption="") {
  ft <- flextable::flextable(df, theme_fun=goc_theme) %>%
    flextable::autofit() %>%
    flextable::colformat_num(big.mark = "") %>%
    flextable::set_caption(
        flextable::as_paragraph(
            flextable::as_chunk(heading, 
                                props=flextable::fp_text_default(font.family="Helvetica", 
                                                    font.size=32)),
            flextable::as_chunk("\n \n", font.size=16),
            flextable::as_chunk(caption, 
                                props=flextable::fp_text_default(font.family="Times", 
                                                    font.size=11, 
                                                    italic=T, ))))
  goc_theme(ft)
}
