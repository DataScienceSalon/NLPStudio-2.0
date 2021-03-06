% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Studio0.R
\docType{class}
\name{Studio0}
\alias{Studio0}
\title{Studio0}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Studio0
}
\arguments{
\item{object}{The object to be processed.}

\item{queue}{The job queue containing processing commands.}
}
\description{
\code{Studio0} Abstract class for TextStudio, DataStudio, and InfoStudio classes.
}
\details{
Abstract class which defines the
}
\section{Studio0 Family of Classes Overview}{

The Studio0 family of classes include the TextStudio the DataStudio,
and the InfoStudio classes, each of which, automate and encapsulate
the processing of text, data, and information or analysis classes.
}

\section{Studio0 methods}{

\strong{Core Methods:}
 \itemize{
  \item{\code{new()}}{Not implemented for this abstract class.}
  \item{\code{addCommand()}}{Method that adds a processing command to the queue. }
  \item{\code{removeCommand()}}{Method that removes a command from the queue.}
  \item{\code{execute()}}{Not implemented for this abstract class. }
  \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
 }
}

\section{Parameters}{

}

\author{
John James, \email{jjames@datasciencesalon.org}
}
\keyword{datasets}
