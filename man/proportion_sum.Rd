\name{proportion_sum}
\alias{proportion_sum}
\title{Proportional Summation}
\description{
Proportionally recalculates numeric variables to sum to a target value.
}
\usage{
proportion_sum(dat, var_list, target, condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be relcalculated}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be recalculated}
  \item{target}{A string, a vector of strings containing variable names, a number, or an expression that the variable(s) will be proportionally recalculated to}
  \item{condition}{An expression in the form of a string used to determine what cases should be recalculated}
}
\details{
proportion_sum recalculates the given variable(s) so that their sum is equal to the value of the target and returns the data with the recalculated variables. If a condition is set, only the cases that meet the condition will be recalculated.
}
\examples{
# Recalculates the values of q1a1, q1a2, and q1a3 so that their sum equals 100
proportion_sum(tempdat, c("q1a1", "q1a2", "q1a3"), 100)

}
