\name{uneven_bind}
\alias{uneven_bind}
\title{Uneven Bind}
\description{
Binds two data frames together that have unequal rows.
}
\usage{
uneven_bind(dat1, dat2, fill = NA)
}
\arguments{
  \item{dat1}{First data frame to be binded}
  \item{dat2}{Second data frame to be binded}
  \item{fill}{Default values of created rows}
}
\details{
uneven_bind combines two data frames together that have an unequal number of rows. Any missing rows will be filled with the given fill argument, defaulting with NA. This function is not intended to merging data sets. Instead, it only binds two data frames together.
}
