#------------------------------------------------------------------------------#
#                                 Reshape                                      #
#------------------------------------------------------------------------------#
#' Reshape
#'
#' \code{Reshape} Reshapes objects into document or sentence level of aggregation.
#'
#' Class responsible for reshaping Corpus and Document object text into a single
#' document level text vector, or sentence vectors.
#'
#' @usage Reshape$new(x, to = c("document", "sentence"))$execute()
#'
#' @template textStudioParams
#' @param to Character vector indicating the level to which the Corpus or
#' Document object should be reshaped. Valid values are "document" and
#' "sentence".  The starting letters, "d", and "s" may also be used.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{Reshape} Returns a Corpus or Document object, with text
#' reshaped accordingly.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
Reshape <- R6::R6Class(
  classname = "Reshape",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..to = character(),

    processDocument = function(document) {

      if (private$..to %in% c("document", "d")) {
        document$content <- paste(document$content, collapse = "")
      } else if (private$..to %in% c("sentence", "s")) {
        document <- Tokenize$new(document, what = "sentence")
      }
      return(document)
    },

    processCorpus = function() {

      documents <- private$..x$getDocuments()

      if (private$..to %in% c("corpus", "c")) {

        # Obtain content
        content <- lapply(documents, function(d) {
          paste(d$content, collapse = "")
        })

        # Remove old documents
        for (i in 1:length(documents)) {
          private$..x$removeDocument(documents[[i]])
        }

        # Create new document
        content <- paste(unlist(content), collapse = "")
        name <- private$..x$getName()
        document <- TextDocument$new(name = name)
        document$content <- content
        private$..x$addDocument(document)

      } else {
        for (i in 1:length(documents)) {
          document <- private$processDocument(documents[[i]])
          private$..x$removeDocument(documents[[i]])
          private$..x$addDocument(document)
        }
      }
      event <- paste0("Reshape performed on Corpus ", private$..x$getName())
      private$..x$log(cls = cls(self)[1], event = event)
      return(private$..x)
    }
  ),

  public = list(
    initialize = function(x, to = "sentence") {

      private$loadDependencies()

      # Validate parameters
      private$..params <- list()
      private$..params$x <- x
      private$..params$discrete$variables <- c('to')
      private$..params$discrete$values <- c(to)
      private$..params$discrete$valid <- list(c("corpus", "document", "sentence", "c", "d", "s"))
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..to <- to

      invisible(self)
    }
  )
)
