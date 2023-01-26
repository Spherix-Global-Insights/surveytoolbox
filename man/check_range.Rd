\name{check_range}
\alias{check_range}
\title{Check Range}
\description{
Checks if any cases are below the minimum or above the maximum.
}
\usage{
check_range(dat, var_list, min = 0, max = 100, fix = FALSE, fix_type = "flat")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{min}{A string, a vector of strings containing variable names, a number, or an expression defining the minimum range each case may be}
  \item{max}{A string, a vector of strings containing variable names, a number, or an expression defining the maximum range each case may be}
  \item{fix}{If TRUE, will automatically recode cases that violate the range. If FALSE, will add to the error report.}
  \item{fix_type}{A string indicating how the issues will be recoded if fix is TRUE}
}
\details{
check_percent checks if any cases in the given variable(s) are below the minimum or above the maximum. If they are and fix is TRUE, they will be recoded according to what fix_type is selected, and the new data will be returned. If fix is FALSE, the function will instead add any cases to the error report.

fix_type has multiple options. By default, "flat" will recode out-of-range values to the minimum or maximum respectively. "mean" will recode out-of-range values to the mean of the values for that variable. The mean is calculated by excluding those values that are out-of-range.
}
\examples{
# Checks all cases in q1 and recodes any that violate the range 0 to 100
check_range(tempdat, "q1", fix = TRUE)

# Check all cases in q5 and q6 and adds any that violate the range 5 to q1 to the error report
check_range(tempdat, c("q5", "q6"), 5, "q1")

}
