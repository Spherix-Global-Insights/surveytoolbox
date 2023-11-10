\name{stack_loops}
\alias{stack_loops}
\title{Stack Loops}
\description{
Stacks loops of repeat columns to create a dataset with more rows.
}
\usage{
stack_loops(dat, first_var, last_var, loop_num)
}
\arguments{
  \item{dat}{A data frame containing the variables to be stacked}
  \item{first_var}{A string containing the name of the variable at the start of the first loop}
  \item{last_var}{A string containing the name of the variable at the end of the first loop}
  \item{loop_num}{The number of times the variables are repeated}
}
\details{
stack_loops takes repeated sets of variables and stacks their rows to return a data frame with one combined set of the repeated variables.

It is necessary that every set of repeated variables are in the same order and contain the same number of variables. Using the placement of the given first variable and last variable in the first set, the function will count the number of variables and stack based off of this number. This function does not rely on matching variable names, so it will work on any data given the proper structure.
}
\examples{
# Stacks 8 sets of variables q1 through q60
stack_loops(tempdat, "q1_1", "q60_1", 8)

}
