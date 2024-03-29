% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_intersection_v.R
\name{make_intersection_v}
\alias{make_intersection_v}
\title{Get a vector of intersection intervals from two vectors of input intervals}
\usage{
make_intersection_v(int1, int2)
}
\arguments{
\item{int1}{vector of intervals}

\item{int2}{another vector of intervals}
}
\value{
a vector of intervals -- each element is the interval which is the intersection of the corresponding input intervals
}
\description{
Note: This is a vectorized version of make.intersection
}
\examples{
int1 <- c(
  lubridate::interval(lubridate::ymd("2023-03-02"), lubridate::ymd("2023-03-05")),
  lubridate::interval(lubridate::ymd("2023-03-06"), lubridate::ymd("2023-03-10")),
  lubridate::interval(lubridate::ymd("2023-03-14"), lubridate::ymd("2023-03-18"))
)
int2 <- c(
  lubridate::interval(lubridate::ymd("2023-03-01"), lubridate::ymd("2023-03-06")),
  lubridate::interval(lubridate::ymd("2023-03-11"), lubridate::ymd("2023-03-12")),
  lubridate::interval(lubridate::ymd("2023-03-13"), lubridate::ymd("2023-03-17"))
)
int3 <- make_intersection_v(int1, int2) # vector of three intervals
}
