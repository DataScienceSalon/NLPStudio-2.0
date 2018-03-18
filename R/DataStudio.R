#==============================================================================#
#                               DataStudio                                     #
#==============================================================================#
#' DataStudio
#'
#' \code{DataStudio} Class for creating data from text
#'
#' The NLPStudio pipeline starts with the TextStudio, a collection of classes
#' that performs preprocessing on text. This class is the second step in the
#' pipeline and is responsible for converting text to data for downstream
#' analysis.
#'
#' @section DataStudio methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a DataStudio.}
#'   \item{\code{token(x, what = c("char", "word", "sentence"))}}{Method for tokenizing an object.}
#'   \item{\code{stem()}}{Method that for removing a command from the queue.}
#'   \item{\code{ngram()}}{Method that executes the job queue. }
#'   \item{\code{dfm()}}{Method that returns the object following execution of the job queue. }
#'   \item{\code{pos()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the DataStudio.}
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
  inherit = Base,

  private = list(
    ..x = character(),
    ..jobQueue = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      # Instantiate variables
      private$..className <- 'DataStudio'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Validation
      private$..params <- list()
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- Clone$new()$corpus(x = x, name = name)

      # Create log entry
      private$..event <- paste0("DataStudio object instantiated.")
      private$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Command Management Methods                       #
    #-------------------------------------------------------------------------#
    addCommand = function(cmd) {

      private$..methodName <- "addCommand"

      if (!c("DataStudio0") %in% class(cmd)) {
        private$..event <- paste0("Invalid text command object. Object must be ",
                                  "of the DataStudio0 classes.  See ?", class(self)[1],
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

      if (!c("DataStudio0") %in% class(cmd)) {
        private$..event <- paste0("Invalid text command object. Object must be ",
                                  "of the DataStudio0 classes.  See ?", class(self)[1],
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
    execute = function() {

      private$..methodName <- "execute"

      for (i in 1:length(private$..jobQueue)) {
        private$..x <- private$..jobQueue[[i]]$execute(private$..x)
      }

      private$..event <- paste0("Processed text processing commands on ",
                                private$..x$getName(), "." )
      private$logIt()

      invisible(self)

    },

    getResult = function() {
      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$DataStudio(self)
    }
  )
)
