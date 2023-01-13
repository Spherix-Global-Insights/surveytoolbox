\name{straightline_analysis}
\alias{straightline_analysis}
\title{Straight-Line Analysis}
\description{
Checks multi-option questions for repeated answers and flags potentially invalid data.
}
\usage{
straightline_analysis(dat, id_var)
}
\arguments{
  \item{dat}{A data frame containing the respondent(s) to be checked}
  \item{id_var}{A string or vector of strings containing the variable name(s) of questions that contain identification data}
}
\details{
straightline_analysis checks for repeated answers in multi-option questions and returns a data frame containing the percentage of questions each respondent straight-lined. This function does not automatically determine which questions are multi-option. Instead, the check_seen_multi function flags questions that are multi-option but not multi-select. This list of flagged questions are then fed into the straightline_analysis function when called.
}
\examples{
# Runs straight-line checks and appends the RESPONDENT_ID variable to the results
straightline_analysis(tempdat, "RESPONDENT_ID")

}
