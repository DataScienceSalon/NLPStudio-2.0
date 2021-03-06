% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextStudio.R
\docType{class}
\name{TextStudio}
\alias{TextStudio}
\title{TextStudio}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
TextStudio
}
\arguments{
\item{object}{The object to be processed.}

\item{queue}{The job queue containing text processing commands.}
}
\description{
\code{TextStudio} Class for performing text cleaning and preprocessing
}
\section{TextStudio Family of Classes Overview}{

The TextStudio family of classes include four types of classes. The manager classes which
are responsible for executing one or more core class objects through the implementation
of a command class object, supported by support class objects.
}

\section{TextStudio methods}{

\strong{Core Methods:}
 \itemize{
  \item{\code{new()}}{Method for instantiating a TextStudio.}
  \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
  \item{\code{removeCommand()}}{Method that removes a command from the queue.}
  \item{\code{execute()}}{Method that executes the job queue. }
  \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
 }
}

\section{Parameters}{

}

\seealso{
Other TextStudio classes: \code{\link{TextStudio0}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
\keyword{datasets}
