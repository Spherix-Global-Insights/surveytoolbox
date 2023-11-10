\name{write_cond_statement}
\alias{write_cond_statement}
\title{Write Conditional Statement}
\description{
Creates a string or vector of strings containing the given variable(s) chained together in conditional statement(s).
}
\usage{
write_cond_statement(var_list, target, test = '==', operator = NULL)
}
\arguments{
  \item{var_list}{A string or vector of strings containing the variable name(s) to be incorporated}
  \item{target}{A string/number or vector of strings/numbers that the variable(s) will be tested against. If multiple strings/numbers are used, the length much match the length of variables exactly.}
  \item{test}{A string or vector of strings containing the type of test(s) the variable(s) will use}
  \item{operator}{A string containing an operator that will be used to separate each conditional statement. If set to NULL, each conditional statement will be returned as a separate string in a vector.}
}
\details{
write_cond_statement is a shortcut function that takes the given variable(s), pairs each of them with the test and target, and returns the statement(s) as a string. If an operator is given, that operator separates each statement to return a long string of chained conditional statements.
}
\examples{
# Returns c("q1a1 == 1", "q1a2 == 2", "q1a3 == 3")
write_cond_statement(c("q1a1", "q1a2", "q1a3"), c(1:3))

# Returns c("q1 < 1", "q2 == 1", "q3 > 1")
write_cond_statement(c("q1", "q2", "q3"), 1, c("<", "==", ">"))

# Returns "q1a1 == 1 & q1a2 == 2 & q1a3 == 3"
write_cond_statement(c("q1a1", "q1a2", "q1a3"), c(1:3), "==", "&")

# Checks q3 on the condition "q1a1 > q2 | q1a2 > q2 | q1a3 > q2"
check_seen(tempdat, "q3", write_cond_statement(c("q1a1", "q1a2", "q1a3"), "q2", ">", "|"))

}
