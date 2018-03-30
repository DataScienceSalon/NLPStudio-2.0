#==============================================================================#
#                               DataStudio                                     #
#==============================================================================#
#' DataStudio
#'
#' \code{DataStudio} Class converts text to data.
#'
#' Class automates the process of converting to text to data via stemming,
#' POS tagging, and the creation of document frequency matrices, ngrams, tokens.
#'
#' @template dataStudioClasses
#'
#' @section DataStudio methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a DataStudio.}
#'   \item{\code{addCommand()}}{Method which adds a data processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a data processing command from the queue.}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' @section Parameters:
#' @param object The object to be processed.
#' @param queue The job queue containing text processing commands.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family DataStudio classes
#' @export
DataStudio <- R6::R6Class(
  classname = "DataStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = NLPStudio0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      # Instantiate variables
      private$..className <- 'DataStudio'
      private$..methodName <- 'initialize'
      private$logR <- LogR$new()

      # Validation
      private$..params <- list()
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x

      # Create log entry
      event <- paste0("DataStudio object instantiated.")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Execute and Get Results                           #
    #-------------------------------------------------------------------------#
    execute = function() {

      private$..methodName <- "execute"

      for (i in 1:length(private$..jobQueue)) {
        data <- private$..jobQueue[[i]]$execute(private$..x)
        private$..x$addData(data)
      }

      event <- paste0("Processed data processing commands for ",
                                private$..x$getName(), "." )
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$dataStudio(self)
    }
  )
)
