\name{check_percent}
\alias{check_percent}
\title{Check Percentage}
\description{
Checks if any cases are above 100 and adjusts them if fix is TRUE.
}
\usage{
check_percent(dat, var_list, fix = TRUE)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{fix}{If TRUE, will automatically recode cases above 100. If FALSE, will add to the error report.}
}
\details{
check_percent checks if any cases in the given variable(s) are above 100. If they are and fix is TRUE, they will be recoded to a number at or below 100 and the new data will be returned. If fix is FALSE, the function will instead add any cases to the error report.
}
\examples{
# Checks all cases in q1 and recodes any above 100
check_percent(tempdat, "q1")

# Check all cases in q1 and q2 and adds any above 100 to the error report
#check_percent(tempdat, c("q1", "q2"), fix = FALSE)

}
