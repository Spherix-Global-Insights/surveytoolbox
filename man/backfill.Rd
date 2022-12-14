\name{backfill}
\alias{backfill}
\title{Backfill}
\description{
Shortcut to backfill all NA within the given variable(s).
}
\usage{
backfill(dat, var_list, replace = 0)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A string or vector of strings containing the variable name(s) to be checked}
  \item{replace}{The number that will replace all NA}
}
\details{
backfill replaces all NA values within the given variable(s) with a chosen numeric value and returns the new data. This function only works with numeric variables. It is designed for quick, simple cases. For more complex replacements, see the recode function.
}
\examples{
# Backfills all NA in the question with 0
backfill(tempdat, "q1")

}
