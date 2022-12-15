\name{create_pct_var}
\alias{create_pct_var}
\title{Create Percentage Variable}
\description{
Shortcut to create new variables by converting given variables into percentages.
}
\usage{
create_pct_var(dat, var_list, total = NULL)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be converted to percentages}
  \item{total}{A string, a vector of strings containing variable names, a number, or an expression that will serve as the divisor for calculating percentages. If NULL is given, the sum of the values of var_list will be used as the divisor instead.}
}
\details{
create_pct_var creates a new variable for every given variable in the var_list by calculating a percentage and returning the data with the variables appended. New variables will retain the same label(s) as the given variables. They will also have the same name, but with the prefix "pct_" appended.

Percentages are calculated by dividing by the given total and multiplying by 100. If no total is given, the sum of the given variable list will be used as the divisor.
}
\examples{
# Creates a new variable called "pct_q1" that is calculated by dividing q1 by q2 and multiplying by 100
create_pct_var(tempdat, "q1", "q2")

# Creates three new variables calculated by dividing by the sum of q1a1, q1a2, and q1a3 and multiplying by 100
create_pct_var(tempdat, c("q1a1", "q1a2", "q1a3"))

}
