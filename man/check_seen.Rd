\name{check_seen}
\alias{check_seen}
\title{Check Seen}
\description{
Checks if a survey question was seen based on a given condition.
}
\usage{
check_seen(dat, var_list, condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{condition}{An expression or vector of expressions in the form of string(s) used to determine when the question should have been seen}
}
\details{
check_seen checks every case within each given variable and determines if a response was provided at the appropriate time based on the condition. If the condition is TRUE but the value is NA, that case is determined to be an error. If the condition is FALSE but the value is not NA, that case is also determined to be an error. Otherwise, that case is not an error.

This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks a single question that should be seen by all respondents
check_seen(tempdat, "q1")

# Checks multiple questions that should only be seen when the respondent is over or equal to the age of 18
check_seen(tempdat, c("q1", "q2", "q3"), "age >= 18")

# Checks multiple questions based on different conditions
check_seen(tempdat, c("q1", "q2", "q3"), c("age >= 18", "q1 > 0", "q1 + q2 == 0"))

}
