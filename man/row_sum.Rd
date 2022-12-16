\name{row_sum}
\alias{row_sum}
\title{Row Sum (Helper)}
\description{
Helper function that sums all elements of the given variable(s) row-wise.
}
\usage{
row_sum(dat, var_list)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be summed}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
}
\details{
row_sum is a helper function that sums every case in the given variable(s) and returns that data frame of summations.
}
\examples{
# Returns the summation of q1a1, q1a2, and q1a3
row_sum(tempdat, c("q1a1", "q1a2", "q1a3"))

}
