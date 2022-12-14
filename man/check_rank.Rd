\name{check_rank}
\alias{check_rank}
\title{Check Rank-Type Question}
\description{
Checks rank-type survey questions for duplicate answers.
}
\usage{
check_rank(dat, var_list)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
}
\details{
check_rank compares every case of every given variable against each other to determine if any duplicate answers were given. If there are duplicate answers, the rank question contains an error.

This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks that there are no duplicate answers among q1a1, q1a2, and q1a3
check_rank(tempdat, c("q1a1", "q1a2", "q1a3"))

}
