#' Make intersection of two intervals
#'
#' @param int1 an interval
#' @param int2 a second interval
#' @importFrom lubridate int_start
#' @importFrom lubridate int_end
#' @importFrom lubridate %--%
#'
#' @return an interval which is the intersection of the two intervals.  If the intervals do not intersect, return NA
#' @export
#'
#' @examples
#' int1 <- lubridate::interval(lubridate::ymd("2023-04-01"), lubridate::ymd("2023-04-10"))
#' int2 <- lubridate::interval(lubridate::ymd("2023-04-03"), lubridate::ymd("2023-04-24"))
#' int3 <- make.intersection(int1, int2) # from 3 to 10

make.intersection <- function(int1,int2){
    if (lubridate::int_start(int1) > lubridate::int_end(int2)) {
        NA
    }
    else if (lubridate::int_start(int2) > lubridate::int_end(int1)) {
        NA
    }
    else {
        max(lubridate::int_start(int1),lubridate::int_start(int2)) %--% min(lubridate::int_end(int1),
                                                                            lubridate::int_end(int2))
    }
}