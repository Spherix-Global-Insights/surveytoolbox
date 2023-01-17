\name{find_var_name}
\alias{find_var_name}
\title{Find Variable Name}
\description{
Searches data frame for variable names that match the given string(s).
}
\usage{
find_var_name(dat, keyword, omit = NULL, all_keyword = TRUE, all_omit = FALSE)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be searched}
  \item{keyword}{A string or vector of strings containing the keyword(s) to look for within variable names}
  \item{omit}{A string or vector of strings containing the keyword(s) to exclude when searching}
  \item{all_keyword}{If TRUE, the function only returns variables that contain ALL given keywords. If FALSE, the function returns variables that contain ANY given keywords.}
  \item{all_omit}{If TRUE, the function only excludes variables that contain ALL given omition keywords. If FALSE, the function excludes variables that contain ANY given omition keywords.}
}
\details{
find_var_name checks each variable name within a data frame for given keywords, excluding given omitions, and returns those variable names as a vector of strings. Multiple keywords and omitions may be input as a vector of strings. all_keyword and all_omit decides how multiple keywords are handled (see above).
}
\examples{
# Returns all variable names that contain "q1" but not "oe"
find_var_name(tempdat, "q1", "oe")

# Combines two functions to quickly check the multiple select question q1
check_seen_multi(tempdat, find_var_name(tempdat, "q1"))

# Returns all variable names that contain "q1" or "q2"
find_var_name(tempdat, c("q1", "q2"), all_keyword = FALSE)

}
