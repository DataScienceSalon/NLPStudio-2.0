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
#' @family Tokens Classes
#' @export
Tokenize <- R6::R6Class(
  classname = "Tokenize",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(
    ..what = character(),
    ..tokensCollection = character(),

    processDocument = function(textDocument) {

      # Obtain metadata and content of TextDocument object
      textId <- textDocument$getId()
      name <- textDocument$getName()
      content <- textDocument$content

      # Produce TokensDocument object content
      if (private$..what %in% c("sentence")) {

        # Use sentence token from openNLP and NLP packages
        s <- paste(content, collapse = "")
        s <- NLP::as.String(s)
        sa <- openNLP::Maxent_Sent_Token_Annotator()
        a <- NLP::annotate(s, sa)
        tokenized <- s[a]

      } else {
        tokenized <- quanteda::tokens(x = content,
                                      what = private$..what)
      }

      # Create TokensDocument object and update content
      tokensDocument <- TokensDocument$new(x = tokenized,
                                           what = private$..what,
                                           textId = textId,
                                           name = name)

      event <- paste0("Tokenized ", textDocument$getName(), " TextDocument.")
      private$logR$log(cls = class(self)[1], event = event)

      return(tokensDocument)
    },

    processCorpus = function(corpus) {

      # Create tokenized documents
      docs <- corpus$getDocuments(cls = "TextDocument")
      tokensDocuments <- lapply(docs, function(d) {
        private$processDocument(d)
      })

      # Add tokensDocuments to Tokens
      for (i in 1:length(tokensDocuments)) {
        private$..tokensCollection$addDocument(tokensDocuments[[i]])
      }
      event <- paste0("Tokenized ", corpus$getName(), " corpus. ")
      private$logR$log(cls = class(self)[1], event = event)
      return(private$..tokensCollection)
    }
  ),

  public = list(
    initialize = function(x, name = NULL, what = NULL) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$x <- x
      private$..params$variables <- c('what')
      private$..params$values <- c(what)
      private$..params$valid <- list(c("word", "sentence",  "character"))
      if (private$validate()$code == FALSE) stop()

      # Initialize private members and Tokens object
      private$..x <- x
      private$..what <- what
      if (is.null(name)) name <- x$getName()

      private$..tokensCollection <- Tokens$new(name = name,
                                                         corpusId = x$getId(),
                                                         what = what)

      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Update
      private$..tokensCollection <- private$processCorpus(private$..x)

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logR$log(cls = class(self)[1], event = event)

      return(private$..tokensCollection)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenize(self)
    }
  )
)
