\name{compare_var}
\alias{compare_var}
\title{Compare Variables}
\description{
Compares two values in the form of variables, list of variables, or expressions based on a given test.
}
\usage{
compare_var(dat, value1, value2, test = '==', condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{value1}{A string, a vector of strings containing variable names, a number, or an expression to be compared against value2}
  \item{value2}{A string, a vector of strings containing variable names, a number, or an expression to be compared against value1}
  \item{test}{A string containing the comparison test that will be used on the two values}
  \item{condition}{An expression in the form of a string used to determine when the question should have been seen}
}
\details{
compare_var compares value1 against value2 using the given test operator when the given condition is met.

Each value will be calculated depending on what is input. If a string containing a variable name is given, the value of each case of that question will be compared. If a vector of strings containing multiple variable names is given, each variable will be summed up row wise. If a number is given, that number will remain as the value compared. If an expression is given, that expression will be evaluated for every case.

This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks if every case in q1 is equal to 100
compare_var(tempdat, "q1", 100)

# Checks that the sum of q1a1, q1a2, and q1a3 is less than or equal to q2
compare_var(tempdat, c("q1a1", "q1a2", "q1a3"), "q2", "<=")

# Checks that q1 is equal to q2 * 2 when age is over or equal to 18
compare_var(tempdat, "q1", "q2 * 2", condition = "age >= 18")

}
