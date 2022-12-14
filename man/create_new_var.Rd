\name{create_new_var}
\alias{create_new_var}
\title{Create New Variable}
\description{
Creates a new variable with defined labels and default values and adds it to the end of the data.
}
\usage{
create_new_var(dat, var_nam, var_lab = "New variable", val_lab = NULL, def_val = NA)
}
\arguments{
  \item{dat}{A data frame containing the variable(s) to be checked}
  \item{var_nam}{A string containing the name of the new variable to be created}
  \item{var_lab}{A string containing the label of the new variable}
  \item{val_lab}{A vector containing the value labels of the new variable. This must be of the form '"label" = value'. For example, c("No" = 0, "Yes" = 1)}
  \item{def_val}{A value or expression in the form of a string that will be the initial value of the new variable}
}
\details{
create_new_var creates a new variable with an equal number of cases to the given data, populates it with labels and values, and returns the data with it appended to the end. A single value or an expression containing a calculation for multiple values may be given as the default value argument to populate the new variable with values.
\examples{
# Creates a new variable with default labels and NA for every case
create_new_var(tempdat, "my_var")

# Creates a new variable containing the result of q1 + q2
create_new_var(tempdat, "sum_var", def_val = "q1 + q2")

# Creates a new variable with labels and a default value of 1 ("Neutral")
create_new_var(tempdat, "agree", "Please rate your level of agreement.", c("Disagree" = 0, "Neutral" = 1, "Agree" = 2), 1)

}
