#==============================================================================#
#                               TextStudio                                     #
#==============================================================================#
#' TextStudio
#'
#' \code{TextStudio} Class for performing text cleaning and preprocessing
#'
#' @template textStudioClasses
#'
#' @section TextStudio methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a TextStudio.}
#'   \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
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
#' @family TextStudio classes
#' @export
TextStudio <- R6::R6Class(
  classname = "TextStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = NLPStudio0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- Clone$new()$corpus(x = x, name = name)

      # Create log entry
      event <- paste0("TextStudio object instantiated.")
      private$logR$log(cls = class(self)[1], event = event)

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

      event <- paste0("Processed text processing commands on ",
                                private$..x$getName(), "." )
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio(self)
    }
  )
)
