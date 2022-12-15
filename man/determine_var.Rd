\name{determine_var}
\alias{determine_var}
\title{Determine Variable (Helper)}
\description{
Helper function used to determine what argument was given as a variable.
}
\usage{
determine_var(dat, var)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var}{A string, a vector of strings containing variable names, a number, or an expression to be evaluated}
}
\details{
determine_var is a helper function used by other functions within the package to determine what should be done with various variable inputs. If a single string containg a variable name is given, it is left alone. If a vector of strings containing variable names is given, they are summed up row wise. If an expression is given, it is evaluated. If a number is given, it is formatted for use by the calling function. Furthermore, a column used for error readability is added to all data types. Finally, this function returns the evaluated/formatted variable.
}
\examples{
# Returns "q1" (no adjustments needed)
determine_var(tempdat, "q1")

# Returns a data frame containing the row wise summations of q1a1, q1a2, and q1a3
determine_var(tempdat, c("q1a1", "q1a2", "q1a3"))

# Returns a data frame containing the evaluated values of (q1+5)/2
determine_var(tempdat, "(q1+5)/2")

# Returns a data frame of 10 repeated the number of rows as the data
determine_var(tempdat, 10)

}
