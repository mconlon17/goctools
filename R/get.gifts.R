#' Get gifts from Flourish
#'
#' @param include.deleted If TRUE, include deleted gifts
#'
#' @return a data.frame of gifts
#' @export
#'
#' @examples
#' get.gifts() # All non-deleted gifts
#' hist(get.gifts()[,c("flo_gift_amount")]) # Histogram of all gift amounts

get.gifts <- function(include.deleted = FALSE) {
    get.table("flo_gifts", include.deleted)
}