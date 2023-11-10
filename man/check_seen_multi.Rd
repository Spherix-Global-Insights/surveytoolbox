\name{check_seen_multi}
\alias{check_seen_multi}
\title{Check Seen Multiple-Select}
\description{
Checks if a multiple-select survey question was seen based on a given condition and fixes common issues.
}
\usage{
check_seen_multi(dat, var_list, condition = "TRUE", sl_flag = TRUE)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_list}{A vector of strings containing the variable names of the multiple-select question to be checked}
  \item{condition}{An expression or vector of expressions in the form of string(s) used to determine when the question should have been seen}
  \item{sl_flag}{Determines whether the question will be marked for a straight-line check or not}
}
\details{
check_seen_multi checks every case within every option of the multiple-select question for errors or backfills issues based on a condition and returns the recoded data. The following lists the possible outcomes and results:

If an option should not have been seen, but there is an answer =0, that answer is recoded to NA.
If an option should have been seen, but there is no answer (NA), that answer is recoded to 0.
If there is a mix of the previous two, a warning message is printed.
If an option should not have been seen, but there is an answer >0, an error is determined.

Also, calling this function on a question will mark that question for a straight-line check. It will group all given variables as a single question for purposes of the check. Any question determined as a multi-select will not be marked. Marking for straight-line checks can be turned off by setting sl_flag to FALSE.
}
\examples{
# Checks a multiple-select question that should be seen by all respondents
tempdat <- check_seen_multi(tempdat, c("q1a1", "q1a2", "q1a3"))

# Checks a multiple-select question that should only be seen when the respondent is over or equal to the age of 18
tempdat <- check_seen_multi(tempdat, c("q1a1", "q1a2", "q1a3"), "age >= 18")

# Checks a multiple-select question with different conditions for each option
tempdat <- check_seen_multi(tempdat, c("q1a1", "q1a2", "q1a3"), c("age >= 18", "age >= 21", "age >= 60"))

}
