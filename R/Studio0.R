#==============================================================================#
#                               Studio0                                        #
#==============================================================================#
#' Studio0
#'
#' \code{Studio0} Abstract class for TextStudio, DataStudio, and InfoStudio classes.
#'
#' Abstract class which defines the
#'
#' @template nlpStudio0Classes
#'
#' @section Studio0 methods:
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
#' @family Studio0 classes
#' @export
Studio0 <- R6::R6Class(
  classname = "Studio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

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

      if (!c("Studio0") %in% class(cmd)) {
        event <- paste0("Invalid text command object. Object must be ",
                                  "of the Studio0 classes.  See ?", class(self)[1],
                                  " for further assistance.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- cmd

      event <- paste0("Added ", cmd$getName(), " to ", private$..x$getName(),
                                " job queue." )
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    },

    removeCommand = function(cmd) {

      private$..methodName <- "removeCommand"

      if (!c("Studio0") %in% class(cmd)) {
        event <- paste0("Invalid text command object. Object must be ",
                                  "of the Studio0 classes.  See ?", class(self)[1],
                                  " for further assistance.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- NULL

      event <- paste0("Removed ", cmd$getName(), " from ", private$..x$getName(),
                                " job queue." )
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                       Execute and Get Results                           #
    #-------------------------------------------------------------------------#
    execute = function() { stop("Not implemented for this abstact class.") }
  )
)
