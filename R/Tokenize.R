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
#' @template dataStudioParams
#' @param what Character string containing either c('character', 'word' ,'sentence)
#' indicating to which format the document should be tokenized.
#' @template dataStudioMethods
#' @template dataStudioClasses
#' @template dataStudioDesign
#'
#' @examples
#'
#' @return \code{Tokenize} A tokenized Corpus, Document, or character text
#' object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @export
Tokenize <- R6::R6Class(
  classname = "Tokenize",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

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

      # Create new document object
      tokenizedDocument <- TextDocument$new(name = document$getName())
      tokenizedTextDocument$content <- tokenized

      event <- paste0("Tokenized ", document$getName(), " document.")
      document$logR$log(cls = class(self)[1], event = event)

      return(tokenizedDocument)
    },

    processCorpus = function(corpus) {

      # Create tokenized documents
      docs <- corpus$getDocuments()
      tokenizedDocuments <- lapply(docs, function(d) {
        private$processDocument(d)
      })

      # Create new Tokens object and add tokenizedDocuments
      tokenizedCorpus <- Tokens$new(name = corpus$getName())
      for (i in 1:length(tokenizedDocuments)) {
        tokenizedCorpus$addDocument(tokenizedDocuments[[i]])
      }
      event <- paste0("Tokenized ", corpus$getName(), " corpus. ")
      private$logR$log(cls = class(self)[1], event = event)
      return(tokenizedCorpus)
    }
  ),

  public = list(
    initialize = function(x, what = NULL) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$x <- x
      private$..params$variables <- c('what')
      private$..params$values <- c(what)
      private$..params$valid <- list(c("word", "w", "sentence", "s", 'NULL'))
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..what <- what

      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Update
      if (class(private$..x)[1] == "Corpus") {
        x <- private$processCorpus(private$..x)
      } else {
        x <- private$processDocument(private$..x)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logR$log(cls = class(self)[1], event = event)

      return(corpus)
    }
  )
)
