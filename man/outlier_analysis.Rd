\name{outlier_analysis}
\alias{outlier_analysis}
\title{Outlier Analysis}
\description{
Checks each numeric question and flags respondents with outliers.
}
\usage{
outlier_analysis(dat, id_var)
}
\arguments{
  \item{dat}{A data frame containing the respondent(s) to be checked}
  \item{id_var}{A string or vector of strings containing the variable name(s) of questions that contain identification data}
}
\details{
outlier_analysis checks every numeric question in the data, flags answers that are outliers, and returns a data frame containing all numeric questions with TRUE or FALSE indicating whether the specific case is an outlier. The function only checks questions that have a variation greater than 5, eliminating multi-select and most scale questions from being flagged. Outliers are determined using the Interquartile Range Criterion.
}
\examples{
# Checks all numeric questions for outliers and appends the RESPONDENT_ID variable to the results
outlier_analysis(tempdat, "RESPONDENT_ID")

}
