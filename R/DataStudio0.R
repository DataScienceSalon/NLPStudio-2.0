#==============================================================================#
#                               DataStudio0                                    #
#==============================================================================#
#' DataStudio0
#'
#' \code{DataStudio0} Abstract class  for the DataStudio family of classes.
#'
#' This abstract class defines a common interface and methods for the DataStudio
#' family of classes.
#'
#' @template dataStudioParams
#' @template dataStudioMethods
#' @template dataStudioClasses
#' @template dataStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio classes
#' @export
DataStudio0 <- R6::R6Class(
  classname = "DataStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..content = list(),
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
      return(x$log(event = event))
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

    settings = function() { private$..settings },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..x)) {
        private$processCorpus()

      } else {
        private$..x <- private$processDocument(private$..x)
      }

      # Log it
      private$..event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logIt()

      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$dataStudio0(self)
    }
  )
)
