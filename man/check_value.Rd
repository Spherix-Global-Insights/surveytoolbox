\name{check_value}
\alias{check_value}
\title{Check Value}
\description{
Checks if a survey question is equal to a given value based on a given condition.
}
\usage{
check_value(dat, var_list, target, test = "==", condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{target}{A value or vector of values containing the target(s) that the variable will be compared to}
  \item{test}{A string or vector of strings containing the test that will be used to compare the variable with the target}
  \item{condition}{An expression or vector of expressions in the form of string(s) used to determine when the question should have been seen}
}
\details{
check_value checks every case within each given variable and determines if an appropriate response was provided at the appropriate time based on the condition. If the condition is TRUE but the value is does not match the target, that case is determined to be an error.

Multiple targets, tests, and conditions can be given to perform varying checks.

This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks that q1 will always equal 10
check_value(tempdat, "q1", 10)

# Checks that q1, q2, q3 will equal 10 when age is great than or equal to 18
check_seen(tempdat, c("q1", "q2", "q3"), 10, "==", "age >= 18")

# Checks multiple questions based on different conditions
check_seen(tempdat, c("q1", "q2", "q3"), c(5, 10, 15), c(">", "==", "<"), c("age >= 18", "q1 > 0", "q1 + q2 == 0"))

}
