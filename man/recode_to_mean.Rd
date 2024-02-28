\name{recode_to_mean}
\alias{recode_to_mean}
\title{Recode to Mean}
\description{
Sets the value of the given variables to the mean.
}
\usage{
recode_to_mean(dat, var_list, condition)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be recoded}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be recoded}
  \item{condition}{An expression or vector of expressions in the form of string(s) used to determine which values will be recoded}
}
\details{
recode_to_mean sets the cases of the given variable(s) to the mean value and returns the data with the variable(s) recoded. The mean value is calculated from all cases excluding the ones that meet the given condition. Only the cases that meet the condition will be recoded.
}
\examples{
# Sets cases when age is over or equal to 18 to the mean of the other cases
tempdat <- recode(tempdat, "q1", "age >= 18")

}
