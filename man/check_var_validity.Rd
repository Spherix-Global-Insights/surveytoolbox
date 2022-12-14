\name{check_var_validity}
\alias{check_var_validity}
\title{Check Variable Validity (Helper)}
\description{
Helper function used to check if a given variable is valid.
}
\usage{
check_var_validity(dat, var_list, type = "any")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{type}{A string containing the type that each variable will be tested for. If "any", all existing variables will be valid. If "num", only numeric variables will be valid. If "str", only string variables will be valid.}
}
\details{
check_var_validity is used in other main functions to determine whether the given variable(s) are valid and usable. It helps in throwing readable error messages when a variable does not exist or does not match the required type.
}
\examples{
# Checks that the given variables are valid and numeric
check_var_validity(tempdat, c("q1", "q2"), type = "num")
}
