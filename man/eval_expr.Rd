\name{eval_expr}
\alias{eval_expr}
\title{Evaluate Expression (Helper)}
\description{
Helper function that evaluates expressions as strings and formats them for calling function use.
}
\usage{
eval_expr(dat, expression)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be evaluated}
  \item{expression}{A string containing the expression to be evaluated}
}
\details{
eval_expr is a helper function that takes an expression in the form of a string, evaluates it, and returns a data frame of the results. If the expression is arithmetic, it will result in a numeric value. If the expression is a condition, it will result in a boolean value. If "TRUE" is given, a data frame with TRUE repeated the number of rows as the given data will be returned.
}
\examples{
# Returns a data frame containing 15 repeated the same number of rows as tempdat
eval_expr(tempdat, "10 + 5")

# Returns a data frame containing booleans indicated whether each case meets the condition q1 == q2
eval_expr(tempdat, "q1 == q2")

}
