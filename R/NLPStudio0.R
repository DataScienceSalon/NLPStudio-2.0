#==============================================================================#
#                               NLPStudio0                                     #
#==============================================================================#
#' NLPStudio0
#'
#' \code{NLPStudio0} Abstract class for TextStudio, DataStudio, and InfoStudio classes.
#'
#' Abstract class which defines the
#'
#' @template nlpStudio0Classes
#'
#' @section NLPStudio0 methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract class.}
#'   \item{\code{addCommand()}}{Method that adds a processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
#'   \item{\code{execute()}}{Not implemented for this abstract class. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' @section Parameters:
#' @param object The object to be processed.
#' @param queue The job queue containing processing commands.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family NLPStudio0 classes
#' @export
NLPStudio0 <- R6::R6Class(
  classname = "NLPStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Base,

  private = list(
    ..x = character(),
    ..jobQueue = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) { stop("Not implemented for this abstract class.")
    },

    #-------------------------------------------------------------------------#
    #                        Command Management Methods                       #
    #-------------------------------------------------------------------------#
    addCommand = function(cmd) {

      private$..methodName <- "addCommand"

      if (!c("NLPStudio00") %in% class(cmd)) {
        private$..event <- paste0("Invalid text command object. Object must be ",
                                  "of the NLPStudio00 classes.  See ?", class(self)[1],
                                  " for further assistance.")
        private$logIt("Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- cmd

      private$..event <- paste0("Added ", cmd$getName(), " to ", private$..x$getName(),
                                " job queue." )
      private$logIt()

      invisible(self)
    },

    removeCommand = function(cmd) {

      private$..methodName <- "removeCommand"

      if (!c("NLPStudio00") %in% class(cmd)) {
        private$..event <- paste0("Invalid text command object. Object must be ",
                                  "of the NLPStudio00 classes.  See ?", class(self)[1],
                                  " for further assistance.")
        private$logIt("Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- NULL

      private$..event <- paste0("Removed ", cmd$getName(), " from ", private$..x$getName(),
                                " job queue." )
      private$logIt()

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                       Execute and Get Results                           #
    #-------------------------------------------------------------------------#
    execute = function() { stop("Not implemented for this abstact class.") }
  )
)
