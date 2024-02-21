\name{check_primary}
\alias{check_primary}
\title{Check Primary-Type Question}
\description{
Checks a primary-type survey question for selections that do not match up with the prior multiple-select question.
}
\usage{
check_primary(dat, var_list, primary, codes = NULL)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A vector of strings containing the variable name(s) of the multiple-select question prior to the primary question}
  \item{primary}{A string or vector of strings containing the variable name(s) of the primary-type question following the mutiple-select question}
  \item{codes}{A vector of numbers that match the coded options of the multiple-select question. This should be left as NULL unless there is only one primary variable}
}
\details{
check_primary checks that every case of a given primary-type question is an option that was selected in the given multiple-select question. If not, that case is considered to be an error.

If the primary question is comprised of only one variable, the function will check the variable accordingly. However, the "codes" argument cannot be left as NULL. Note: the codes argument must match the number of codes and the value of the coded options from the multiple-select question exactly.

This function does not return any arguments, but instead adds to the error_report or prints an error report depending on the set preferences.
}
\examples{
# Checks that the primary variables will only have values if their respective variables have been selected
check_primary(tempdat, c("q1a1", "q1a2", "q1a3"), c("q1a1_primary", "q1a2_primary", "q1a3_primary"))

# Checks that q1_primary will have the value 1, 2, or 3 depending on what was chosen in q1a1, q1a2, and q1a3
check_primary(tempdat, c("q1a1", "q1a2", "q1a3"), "q1_primary", c(1:3))

}
