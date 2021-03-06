% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TokensDocument.R
\docType{class}
\name{TokensDocument}
\alias{TokensDocument}
\title{TokensDocument}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
myTokensDocument <- TokensDocument$new(x = tokenizedText, name = "skiReport")
}
\arguments{
\item{x}{Character vector or vectors containing tokenized text.}

\item{what}{Character string specifying the level of tokenizization.}

\item{name}{Character string containing the name for the TokensDocument object.}

\item{key}{Character string or character vector containing the key or
keys associated a metadata field or fields.}

\item{value}{Character string or character vector, equal in length to
the key vector, containing the metadata value or values.}

\item{meta}{Logical.  If TRUE, object metadata are included in the summary.}

\item{stats}{Logical.  If TRUE, object descriptive statistics are included in the summary.}

\item{state}{Logical. If TRUE, object state information is included in the summary.}

\item{system}{Logical. If TRUE, system information is included in the summary.}

\item{quiet}{Logical, If TRUE, summary is not printed to console.}

\item{abbreviated}{Logical.  If TRUE, a one row data frame containing key metadata is returned.}
}
\value{
TokensDocument object, containing the tokenized text.
}
\description{
\code{TokensDocument} Domain class containing a tokenized Document object.
}
\details{
Class containing document level text tokenized via the Tokenize class. Objects are aggregated
at the corpus level via the Tokens class.
}
\section{Core Methods}{

 \itemize{
  \item{\code{new(x, what = c("sentence", "word", "character"), name = NULL)}}{Initializes an object of the TokensDocument class.}
  \item{\code{content(value)}}{Active binding method for updating TokensDocument content.}
  \item{\code{summary(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
  quiet = FALSE, abbreviated = FALSE)}}{Summarizes TokensDocument object.}
 }
}

\section{Metadata Method}{

 \itemize{
  \item{\code{meta(key = NULL, value = NULL)}}{Provides facility for
  managing an object's metadata represented as key/value pairs in
  a list format. Metadata may be indicated via single key and
  value character strings or by pairs of character vectors. If
  no parameters are passed to the method, then it returns the
  current metadata, as well as system generated application
  and system metadata, in data frame format. If a character
  string or vector is passed via the key parameter, the value
  or values associated with the keys are returned.}
  \item{\code{log(cls = class(self)[1], event = NULL)}}{Class for posting to and
  retrieving an object's log. If the event parameter is provided,
  the event is posted to the object's log and the log is returned
  invisibly.  If the event parameter is NULL, the existing log
  is returned.}
}
}

\seealso{
Other Data Classes: \code{\link{TokensCollection}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
\keyword{datasets}
