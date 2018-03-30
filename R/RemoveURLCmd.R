#------------------------------------------------------------------------------#
#                             RemoveURLCmd                                     #
#------------------------------------------------------------------------------#
#' RemoveURLCmd
#'
#' \code{RemoveURLCmd} Command for the RemoveURL class.
#'
#' Class that encapsulates the command to execute an object of the RemoveURL
#' class
#'
#' @usage RemoveURLCmd$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveURLCmd <- R6::R6Class(
  classname = "RemoveURLCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveURLCmd"
      private$..methodName <- "initialize"
      private$..meta$core$name <- private$..className
      private$logR  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveURL$new(x)$execute()
      return(x)
    }
  )
)
