\name{check_seen_multi}
\alias{check_seen_multi}
\title{Check Seen Multiple-Select}
\description{
Checks if a multiple-select survey question was seen based on a given condition and fixes common issues.
}
\usage{
check_seen_multi(dat, var_list, condition = "TRUE")
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A vector of strings containing the variable names of the multiple-select question to be checked}
  \item{condition}{An expression in the form of a string used to determine when the question should have been seen}
}
\details{
check_seen_multi checks every case within every option of the multiple-select question for errors or backfills issues based on a condition and returns the recoded data. The following lists the possible outcomes and results:

If an option should not have been seen, but there is an answer =0, that answer is recoded to NA.
If an option should have been seen, but there is no answer (NA), that answer is recoded to 0.
If there is a mix of the previous two, a warning message is printed.
If an option should not have been seen, but there is an answer >0, an error is determined.
}
\examples{
# Checks a multiple-select question that should be seen by all respondents
check_seen_multi(tempdat, c("q1a1", "q1a2", "q1a3"))

# Checks a multiple-select question that should only be seen when the respondent is over or equal to the age of 18
check_seen_multi(tempdat, c("q1a1", "q1a2", "q1a3"), "age >= 18")

}
