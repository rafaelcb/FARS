% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Make filename for Fatality Analysis Reporting System data}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A numeric value indicating the year for which the data is going
to be read.}
}
\value{
A string containing the name of the file with FARS data for the input
  year.
}
\description{
This function creates a filename corresponding to the Fatality Analysis
Reporting System (FARS) csv files for a specific year.
}
\note{
The csv files with the data need to be downloaded previously from the
  US National Highway Traffic Safety Administration's Fatality Analysis
  Reporting System website
  (\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
}
\examples{
make_filename(2010)
make_filename(2015)

}
