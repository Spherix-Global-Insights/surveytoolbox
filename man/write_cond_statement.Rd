\name{write_cond_statement}
\alias{write_cond_statement}
\title{Write Conditional Statement}
\description{
Creates a string containing the given variable(s) chained together as a conditional statement.
}
\usage{
write_cond_statement(var_list, target, test = '==', operator = "|")
}
\arguments{
  \item{var_list}{A string or vector of strings containing the variable name(s) to be incorporated}
  \item{target}{A string/number or vector of strings/numbers that the variable(s) will be tested against. If multiple strings/numbers are used, the length much match the length of variables exactly.}
  \item{test}{A string containing the type of test the variable(s) will use}
  \item{operator}{A string containing an operator that will be used to separate each conditional statement}
}
\details{
write_cond_statement is a shortcut function that takes the given variable(s), pairs each of them with the test and target, and separates each statement with the operator to return a long string of chained conditional statements.
}
\examples{
# Returns "q1a1 == 1 | q1a2 == 2 | q1a3 == 3"
write_cond_statement(c("q1a1", "q1a2", "q1a3"), c(1:3))

# Checks q3 on the condition "q1a1 > q2 | q1a2 > q2 | q1a3 > q2"
check_seen(tempdat, "q3", write_cond_statement(c("q1a1", "q1a2", "q1a3"), "q2", ">")

}
