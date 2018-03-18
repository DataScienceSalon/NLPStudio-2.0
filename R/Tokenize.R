#------------------------------------------------------------------------------#
#                                  Tokenize                                    #
#------------------------------------------------------------------------------#
#' Tokenize
#'
#' \code{Tokenize}  Tokenizes text into character, word, or sentence tokens.
#'
#' This class is a wrapper for the  \code{\link[quanteda]{tokens}} function for
#' character, and word, tokenization. Sentence tokenization functionality is
#' provided using the openNLP package.
#'
#' Sources:
#' \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#' \url{https://cran.r-project.org/web/packages/openNLP/openNLP.pdf}
#'
#' @usage Tokenize$new(x, what = c("sentence", "word"))$execute()
#'
#' @template textStudioParams
#' @param what Character string containing either c('character', 'word' ,'sentence)
#' indicating to which format the document should be tokenized.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{Tokenize} A tokenized Corpus, Document, or character text
#' object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
Tokenize <- R6::R6Class(
  classname = "Tokenize",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..what = character(),

    processDocument = function(document) {

      private$..method <- "processDocument"

      content <- document$content

      # Produce data object content
      if (private$..what %n% c("sentence", "s")) {

        # Use sentence token from openNLP and NLP packages
        s <- paste(content, collapse = "")
        s <- NLP::as.String(s)
        sa <- openNLP::Maxent_Sent_Token_Annotator()
        a <- NLP::annotate(s, sa)
        tokenized <- s[a]

      } else {
        tokenized <- quanteda::tokens(x = content, what = private$..what)
      }

      # Create Data Object and add to Document object
      id <- paste0(class(self)[1], "-", private$..what)
      data <- Data$new(id = id, content = tokenized)
      document$addDNA(data)

      private$..event <- paste0("Tokenized ", document$getName(), " document.")
      private$logIt()

      return(document)
    },

    processCorpus = function(corpus) {

      private$..method <- "processCorpus"
      docs <- corpus$get()
      lapply(docs, function(d) {
        corpus$addDocument(private$processDocument(d))
      })
      private$..event <- paste0("Tokenized ", corpus$getName(), " corpus. ")
      private$logIt()
      return(corpus)
    }
  ),

  public = list(
    initialize = function(x, what = NULL) {
      private$..className <- "Tokenize"
      private$..methodName <- "initialize"
      private$..meta$object$name <-  "Tokenize"
      private$..logs  <- LogR$new()

      private$..params <- list()
      private$..params$x <- x
      private$..params$variables <- c('what')
      private$..params$values <- c(what)
      private$..params$valid <- list(c("word", "w", "sentence", "s", 'NULL'))
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..what <- what

      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Update
      corpus <- private$processCorpus(private$..x)

      # Log it
      private$..event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logIt()

      return(corpus)
    },

    accept = function(visitor)  {
      visitor$tokenize(self)
    }
  )
)
