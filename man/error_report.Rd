\name{error_report}
\alias{error_report}
\title{Error Report (Helper)}
\description{
Helper function used to produce readable error reports.
}
\usage{
error_report(dat, variables, bools)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) with errors}
  \item{variables}{A data frame containing the variable(s) found with errors}
  \item{bools}{A data frame containing the boolean values of which cases are errors}
}
\details{
NOTE: error_report is a helper function designed solely for use by main functions within the package. It requires specific formatting and will not produce meaningful results if called incorrectly.
}
