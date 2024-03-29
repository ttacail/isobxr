% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_functions.R
\name{time_converter}
\alias{time_converter}
\title{Convert time units in a dataframe column}
\usage{
time_converter(
  dataframe,
  time_colname,
  conv_timecolname,
  former_unit,
  new_unit
)
}
\arguments{
\item{dataframe}{dataframe for which a column with numerical time values should be converted.}

\item{time_colname}{name of column with time (numerical) values to be converted to a different time unit. \cr
(character string)}

\item{conv_timecolname}{name of the column after time units conversion.  \cr
Can be identical to time_colname. \cr
(character string)}

\item{former_unit}{former time unit. Character string among the following: \cr
\emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}}

\item{new_unit}{new time unit. Character string among the following: \cr
\emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}}
}
\value{
a dataframe with values converted to new time unit.
}
\description{
Convert the time units in plots
}
\keyword{internal}
