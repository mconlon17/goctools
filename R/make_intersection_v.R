#' Get a vector of intersection intervals from two vectors of input intervals
#'
#' Note: This is a vectorized version of make.intersection
#'
#' @param int1 vector of intervals
#' @param int2 another vector of intervals
#'
#' @return a vector of intervals -- each element is the interval which is the intersection of the corresponding input intervals
#' @export
#'
#' @examples
#' int1 <- c(
#'   lubridate::interval(lubridate::ymd("2023-03-02"), lubridate::ymd("2023-03-05")),
#'   lubridate::interval(lubridate::ymd("2023-03-06"), lubridate::ymd("2023-03-10")),
#'   lubridate::interval(lubridate::ymd("2023-03-14"), lubridate::ymd("2023-03-18"))
#' )
#' int2 <- c(
#'   lubridate::interval(lubridate::ymd("2023-03-01"), lubridate::ymd("2023-03-06")),
#'   lubridate::interval(lubridate::ymd("2023-03-11"), lubridate::ymd("2023-03-12")),
#'   lubridate::interval(lubridate::ymd("2023-03-13"), lubridate::ymd("2023-03-17"))
#' )
#' int3 <- make_intersection_v(int1, int2) # vector of three intervals
make_intersection_v <- function(int1, int2) {
  class(int1)
  for (i in seq(length(int1))) {
    i1 <- int1[i]
    i2 <- int2[i]
    int1[i] <- make_intersection(i1, i2)
  }
  int1
}
