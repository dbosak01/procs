% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proc_compare.R
\name{proc_compare}
\alias{proc_compare}
\title{Reports on the differences between two datasets}
\usage{
proc_compare(
  data1,
  data2,
  precision = NULL,
  report_type = NULL,
  report_location = NULL
)
}
\arguments{
\item{data1}{The first dataset to compare.}

\item{data2}{The second dataset to compare.}

\item{precision}{The variables to sort by.}

\item{report_type}{The output type of the report to create.  Valid values
are 'TXT', 'PDF', 'RTF', 'HTML' and 'DOCX'.}

\item{report_location}{The the output path for the comparison report.}
}
\value{
A list of datasets describing the results of the comparison.
}
\description{
Here is a description
}
\details{
Here is some details
}
