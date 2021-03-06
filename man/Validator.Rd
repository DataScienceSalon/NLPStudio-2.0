% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Validator.R
\docType{class}
\name{Validator}
\alias{Validator}
\title{Validator}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Validator
}
\arguments{
\item{object}{Originator object}
}
\value{
A list containing:
\itemize{
 \item status: Logical. If validation passed, then, TRUE, otherwise FALSE
 \item msg: Character string describing the error if status is FALSE
}
}
\description{
Class accepts validation requests from client objects and dispatches the
appropriate Visitor class.
}
\details{
\code{Validator} Class responsible for validation of requests pertaining to:
\itemize{
 \item Object instantiation
 \item Composition and Aggregation: Requests to manipulate aggregate and composite objects
 \item Metadata: Enforces metadata consistency
}
}
\section{Validator methods}{

This section summarizes the methods in the Validator class.
\describe{
 \item{\code{init(object)}}{Dispatches the initialization validation visitor via the accept method of object.}
 \item{\code{attach(object)}}{Dispatches the attach validation visitor via the accept method of object.}
 \item{\code{meta(object)}}{Dispatches the meetadata validation visitor via the accept method of object.}
 }
}

\author{
John James, \email{jjames@datasciencesalon.org}
}
\keyword{datasets}
