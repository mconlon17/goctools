#' List Gifts by Payment Method and Time
#'
#' @param by.year If TRUE, table is one row per year.  If FALSE, result is by year/month
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#'
#' @return a flextable with gifts counted by payment type and time (year or year/month)
#' @export
#'
#' @examples
#' list_gifts_by_payment_method_and_time()
list_gifts_by_payment_method_and_time <- function(by.year = T) {
  Year <- YearMonth <- flo_gift_date <- flo_gift_payment_method <- n <- NULL

  gifts <- get_gifts() %>%
    dplyr::mutate(Year = year(as_date(flo_gift_date)), YearMonth = rollback(as_date(flo_gift_date), roll_to_first = TRUE))

  if (by.year) {
    tab <- gifts %>%
      dplyr::group_by(Year, flo_gift_payment_method) %>%
      dplyr::tally() %>%
      dplyr::group_by(Year) %>%
      tidyr::pivot_wider(names_from = flo_gift_payment_method, values_from = n) %>%
      dplyr::arrange(desc(Year))

    ft <- goc_table(tab, "Gifts by Payment Method and Year")
  } else {
    tab <- gifts %>%
      dplyr::group_by(YearMonth, flo_gift_payment_method) %>%
      dplyr::tally() %>%
      dplyr::group_by(YearMonth) %>%
      tidyr::pivot_wider(names_from = flo_gift_payment_method, values_from = n) %>%
      dplyr::arrange(desc(YearMonth))

    ft <- goc_table(tab, "Gifts by Payment Method and Year/Month")
  }

  ft
}
