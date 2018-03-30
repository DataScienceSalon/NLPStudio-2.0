#==============================================================================#
#                               TextStudio0                                    #
#==============================================================================#
#' TextStudio0
#'
#' \code{TextStudio0} Abstract class  for the TextStudio family of classes.
#'
#' This abstract class defines a common interface and methods for the TextStudio
#' family of classes.
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio classes
#' @export
TextStudio0 <- R6::R6Class(
  classname = "TextStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..x = character(),
    ..regex = character(),
    ..replacement = character(),
    ..params = list(
      x = character(),
      pattern = character(),
      replacement = character(),
      logicals = list(
        variables = character(),
        values = character()
      ),
      discrete = list(
        variables = character(),
        values = character(),
        valid = character()
      )
    ),

    logEvent = function(x) {
      event <- paste0(private$..className, " object execution, complete.")
      return(x$log(cls = class(self)[1], event = event))
    },

    processDocument = function(document) {
      document$content <- gsub(private$..regex,
                      private$..replacement,
                      document$content, perl = TRUE)
      document <- private$logEvent(document)
      return(document)
    },

    processCorpus = function() {
      docs <- private$..x$getDocument()
      for (i in 1:length(docs)) {
        doc <- private$processDocument(docs[[i]])
        private$..x$attach(doc)
      }
      private$..x <- private$logEvent(private$..x)
      return(TRUE)
    }
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..x)) {
        private$processCorpus()

      } else {
        private$..x <- private$processDocument(private$..x)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logR$log(cls = class(self)[1], event = event)

      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio0(self)
    }
  )
)
