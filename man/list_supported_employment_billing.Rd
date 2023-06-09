% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_supported_employment_billing.R
\name{list_supported_employment_billing}
\alias{list_supported_employment_billing}
\title{Supported Employment Billing List}
\usage{
list_supported_employment_billing(date = Sys.Date())
}
\arguments{
\item{date}{Date in month for billing list}
}
\value{
a flextable containing the data required to prepare grant billing for supported employment
}
\description{
Supported Employment Billing List
}
\examples{
list_supported_employment_billing()
list_supported_employment_billing("2023-04-05")
}
