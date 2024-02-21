\name{between}
\alias{between}
\title{Between}
\description{
Returns a list of all of the variable names between the given variables.
}
\usage{
between(dat, first_var, last_var)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be searched}
  \item{first_var}{A string containing the name of the first variable of the list of variables}
  \item{last_var}{A string containg the name of the last variable of the list of variables}
}
\details{
between takes a first and last variable and returns every variable name, including the two given, between those two variables. The returned value will be in the form of a vector of strings.
}
\examples{
# Returns all variable names q1, q2, q3, q4, q5
between(tempdat, "q1", "q5")

}
