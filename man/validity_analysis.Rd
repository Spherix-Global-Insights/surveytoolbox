\name{validity_analysis}
\alias{validity_analysis}
\title{Validity Analysis}
\description{
Runs a few tests to find respondents with potentially invalid data.
}
\usage{
validity_analysis(dat, id_var)
}
\arguments{
  \item{dat}{A data frame containing the respondent(s) to be checked}
  \item{id_var}{A string or vector of strings containing the variable name(s) of questions that contain identification data}
}
\details{
validity_analysis runs the straightline_analysis and outlier_analysis and returns a combined data frame containing the results. These results are meant as a tool to help determine which respondents have invalid data and should be considered for removal. The straightline_analysis and outlier_analysis do not need to be run if validity_analysis is run. See their respective entries for more information about how they work.
}
\examples{
# Runs validity tests and appends the RESPONDENT_ID variable to the results
valid <- validity_analysis(tempdat, "RESPONDENT_ID")

}
