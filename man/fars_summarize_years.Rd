% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarise FARS data according to observations number}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{years required}
}
\value{
This function returns data frame with table of month/year pairs quantity for FARS data
}
\description{
This is the function to get all month/year pairs for specified \code{years} range
and then summarise according to them.
}
\examples{
fars_summarize_years(2013:2015)
fars_summarize_years(c(2013, 2015))

}
