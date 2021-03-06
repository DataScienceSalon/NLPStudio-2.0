% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSource.R
\docType{class}
\name{CSource}
\alias{CSource}
\title{CSource}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
myCorpus <- CSource$new(x)$vector()
}
\arguments{
\item{x}{Character vector containing text, the name of a file, or directory
or a corpus object from a supported package.}

\item{name}{Character vector containing the name to assign to the Corpus object.}

\item{concatenate}{Logical for the vector method. If TRUE, character vectors are
concatenated into a single text for the Document object.}
}
\value{
Corpus object.
}
\description{
\code{CSource} Class for sourcing Corpus objects.
}
\details{
CSource is the client facing class used to source a Corpus object from a variety
of formats. Methods support sourcing Corpora objects from character vectors,
JSON, and XML files or directories. Corpora may be imported from other
packages such as Quanteda, TM, KoRpus, and qdap.
}
\section{Methods}{

 \itemize{
  \item{\code{new()}}{Initializes an object of the CSource class.}
  \item{\code{vector(x, name = NULL)}}{Sources the Corpus object from a character vector.}
  \item{\code{directory(x, name = NULL)}}{Sources the Corpus object from a directory.}
  \item{\code{json(x, name = NULL)}}{Sources the Corpus object from a json source.}
  \item{\code{xml(x, name = NULL)}}{Sources the Corpus object from an xml source.}
  \item{\code{quanteda(x, name = NULL)}}{Sources the Corpus object from a Quanteda corpus object.}
  \item{\code{tm(x, name = NULL)}}{Sources the Corpus object from a tm VCorpus object. }
  \item{\code{korpus(x, name = NULL)}}{Sources the Corpus object from a koRpus corpus object.}
  \item{\code{qdap(x, name = NULL)}}{Sources the Corpus object from a qdap corpus object.}
 }
}

\examples{
corpus <- CSource$new()$vector(x)
corpus <- CSource$new()$file(x)
corpus <- CSource$new()$directory(x)
corpus <- CSource$new()$json(x)
corpus <- CSource$new()$xml(x)
corpus <- CSource$new()$quanteda(x)
corpus <- CSource$new()$tm(x)
corpus <- CSource$new()$koRpus(x)
corpus <- CSource$new()$qdap(x)


}
\seealso{
Other Corpus Source Classes: \code{\link{CSource0}},
  \code{\link{CSourceDir}}, \code{\link{CSourceQuanteda}},
  \code{\link{CSourceTM}}, \code{\link{CSourceVector}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
\keyword{datasets}
