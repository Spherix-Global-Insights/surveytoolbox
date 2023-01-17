\name{check_range}
\alias{check_range}
\title{Check Range}
\description{
Checks if any cases are below the minimum or above the maximum.
}
\usage{
check_range(dat, var_list, min = 0, max = 0, fix = FALSE)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{min}{A number defining the minimum range each case may be}
  \item{max}{A number defining the maximum range each case may be}
  \item{fix}{If TRUE, will automatically recode cases that violate the range. If FALSE, will add to the error report.}
}
\details{
check_percent checks if any cases in the given variable(s) are below the minimum or above the maximum. If they are and fix is TRUE, they will be recoded to the minimum or maximum respectively, and the new data will be returned. If fix is FALSE, the function will instead add any cases to the error report.
}
\examples{
# Checks all cases in q1 and recodes any that violate the range 0 to 100
check_range(tempdat, "q1", fix = TRUE)

# Check all cases in q1 and q2 and adds any that violate the range 5 to 450 to the error report
check_pct(tempdat, c("q1", "q2"), 5, 450)

}
