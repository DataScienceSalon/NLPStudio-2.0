% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Validators.R
\name{validateDiscrete}
\alias{validateDiscrete}
\title{validateDiscrete}
\usage{
validateDiscrate(params)
}
\arguments{
\item{object}{The object to be validated}
}
\value{
a list containing two elements: a code and a message. If the
validation passed, the code will be TRUE and the msg element will be NULL.
Otherwise, the code will be FALSE and the msg element will describe
the error.
}
\description{
\code{validateDiscrete} Validates a parameters that take discrete values.
}
\details{
Class validates objects which take discrete parameters. The object must
have a public member called params which is a list containing three
elements: (1) a vector of variable names, (2) a vector containing
the values of the variables, and (3) a list of vectors, where each vector
contains the valid values for each of the variables.
}
\seealso{
Other Validation Functions: \code{\link{validateKeyValue}},
  \code{\link{validateLogical}}
}
\author{
John James, \email{jjames@DataScienceSalon.org}
}
