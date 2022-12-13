\name{check_seen}
\alias{check_seen}
\title{Check Seen}
\description{
Checks if a survey question was seen based on a given condition.
}
\usage{
check_seen(x, ..., condition = "TRUE")
}
\arguments{
  \item{x}{A data frame containing the variable(s) to be checked}
  \item{...}{A string or vector of strings containing the variable name(s) to be checked}
  \item{condition}{An expression in the form of a string used to determine when the question should have been seen}
}
\details{
check_seen checks every case within each given variable and determines if a response was provided at the appropriate time based on the condition. If the condition is TRUE but the value is NA, that case is determined to be an error. If the condition is FALSE but the value is not NA, that case is also determined to be an error. Otherwise, that case is not an error.
\n
This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks a single question that should be seen by all respondents
check_seen(tempdat, "q1")

# Checks multiple questions that should only be seen when the respondent is over or equal to the age of 18
check_seen(tempdat, c("q1", "q2", "q3"), "age >= 18")

}
