#' Get gifts from Flourish
#'
#' @param include.deleted If TRUE, include deleted gifts
#'
#' @return a data.frame of gifts
#' @export
#'
#' @examples
#' get_gifts() # All non-deleted gifts
#' hist(get_gifts()[, c("flo_gift_amount")]) # Histogram of all gift amounts
get_gifts <- function(include.deleted = FALSE) {
  get_table("flo_gifts", include.deleted)
}
