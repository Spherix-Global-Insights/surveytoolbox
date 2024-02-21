\name{recode}
\alias{recode}
\title{Recode}
\description{
Sets the value of the given variables to a new value.
}
\usage{
recode(dat, var_list, target, condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be recoded}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be recoded}
  \item{target}{A string, a vector of strings containing variable names, a number, or an expression that the variable(s) will be recoded to}
  \item{condition}{An expression or vector of expressions in the form of string(s) used to determine which values will be recoded}
}
\details{
recode sets the cases of the given variable(s) to a new value given as the target and returns the data with the variable(s) recoded. If a condition is given, only the cases where the condition is met will be recoded.
}
\examples{
# Sets every value of q1 to half
tempdat <- recode(tempdat, "q1", "q1/2")

# Sets the values of q1 and q2 to 10 when age is over or equal to 18
tempdat <- recode(tempdat, c("q1", "q2"), 10, "age >= 18")

# Sets the values of q1 and q2 to 10 based off of varying conditions per question
tempdat <- recode(tempdat, c("q1", "q2"), 10, C("age >= 18", "q1 > 0"))

}
