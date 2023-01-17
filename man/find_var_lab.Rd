\name{find_var_lab}
\alias{find_var_lab}
\title{Find Variable Label}
\description{
Searches data frame for variable labels that match the given string(s).
}
\usage{
find_var_lab(dat, keyword, omit = NULL, all_keyword = TRUE, all_omit = FALSE)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be searched}
  \item{keyword}{A string or vector of strings containing the keyword(s) to look for within variable labels}
  \item{omit}{A string or vector of strings containing the keyword(s) to exclude when searching}
  \item{all_keyword}{If TRUE, the function only returns variables that contain ALL given keywords. If FALSE, the function returns variables that contain ANY given keywords.}
  \item{all_omit}{If TRUE, the function only excludes variables that contain ALL given omition keywords. If FALSE, the function excludes variables that contain ANY given omition keywords.}
}
\details{
find_var_nam checks each variable label within a data frame for given keywords, excluding given omitions, and returns the names of those variables as a vector of strings. Multiple keywords and omitions may be input as a vector of strings. all_keyword and all_omit decides how multiple keywords are handled (see above).

Please note, this function is much slower than find_var_name because it uses the get_label function on every variable and tends to check against larger strings. Also, just like find_var_name, this function is case sensitive.
}
\examples{
# Returns all variable names for variable labels that contain "age" but not "time"
find_var_lab(tempdat, "age", "time")

}
