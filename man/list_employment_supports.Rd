% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_employment_supports.R
\name{list_employment_supports}
\alias{list_employment_supports}
\title{List Employment Supports}
\usage{
list_employment_supports(date = Sys.Date())
}
\arguments{
\item{date}{Supports will be listed for 90 days up to and including the specified date}
}
\value{
a flextable with the desired employment supports
}
\description{
List Employment Supports
}
\examples{
list_employment_supports()
list_employment_supports("2022-12-31")
}
