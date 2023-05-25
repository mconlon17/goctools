#' Visitor's Sign In Sheet
#'
#' @param date The date for the sign in sheet
#' @importFrom dplyr add_row
#' @importFrom flextable flextable
#' @importFrom flextable width
#' @importFrom flextable set_header_labels
#' @importFrom flextable set_caption
#'
#' @return a flextable with the visitor's sign in sheet for the requested date
#' @export
#'
#' @examples
#' list_visitors_sign_in()
#' list_visitors_sign_in("2015-07-04")
list_visitors_sign_in <- function(date = Sys.Date()) {
  tab <- data.frame(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
    dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "")

  ft <- goc_table(tab, paste0("Visitor Sign-In Sheet for ", date)) %>%
    flextable::width(j = 1, 3) %>%
    flextable::width(j = seq(3, 8), width = 0.8) %>%
    flextable::width(j = 2, width = 2.5) %>%
    flextable::set_header_labels(In.2 = "In", Out.2 = "Out", In.3 = "In", Out.3 = "Out")
  ft
}
