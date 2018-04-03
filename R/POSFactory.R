#------------------------------------------------------------------------------#
#                                  POSFactory                                  #
#------------------------------------------------------------------------------#
#' POSFactory
#'
#' \code{POSFactory} Class responsible for creating a POSCollection object from Corpus objects.
#'
#' This class is a wrapper for the  \code{\link[openNLP]{Maxent_POS_Tag_Annotator}}
#' function in the openNLP package.
#'
#' Sources:
#' \url{https://cran.r-project.org/web/packages/openNLP/openNLP.pdf}
#'
#' @usage POSFactory$new(x)$execute()
#'
#' @template dataStudioParams
#' @template dataStudioMethods
#' @template dataStudioClasses
#' @template dataStudioDesign
#'
#' @return \code{POSCollection} object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @family POS Classes
#' @export
POSFactory <- R6::R6Class(
  classname = "POSFactory",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(

    ..posCollection = character(),

    processDocument = function(textDocument) {

      # Obtain metadata and content of TextDocument object
      textId <- textDocument$getId()
      name <- textDocument$getName()
      content <- textDocument$content

      # Prepare text
      s <- paste(content, collapse = "")
      s <- NLP::as.String(s)

      # Obtain word, sentence, and POS annotators
      sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
      word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
      pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator()
      a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
      a3 <- annotate(s, pos_tag_annotator, a2)
      a3w <- subset(a3, type == "word")
      tags <- sapply(a3w$features, `[[`, "POS")

      # Create POSDocument object
      posDocument <- POSDocument$new(x = tags, textId = textId, name = name)

      return(posDocument)
    },

    processCorpus = function(corpus) {

      # Create tokenized documents
      docs <- corpus$getDocuments(cls = "TextDocument")
      posDocuments <- lapply(docs, function(d) {
        private$processDocument(d)
      })

      # Add posDocuments to POSCollection
      for (i in 1:length(posDocuments)) {
        private$..posCollection$addDocument(posDocuments[[i]])
      }
      return(private$..posCollection)
    }
  ),

  public = list(
    initialize = function(x, name = NULL) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      # Initialize private members and Tokens object
      private$..x <- x
      if (is.null(name)) name <- x$getName()

      private$..posCollection <- POSCollection$new(name = name,
                                                   corpusId = x$getId())
      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Update
      private$..posCollection <- private$processCorpus(private$..x)

      return(private$..posCollection)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$posFactory(self)
    }
  )
)
