#------------------------------------------------------------------------------#
#                            RemoveHyphensCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveHyphensCmd
#'
#' \code{RemoveHyphensCmd} Command for the RemoveHyphens class.
#'
#' Class that encapsulates the command to execute an object of the RemoveHyphens
#' class
#'
#' @usage RemoveHyphensCmd$new()
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
RemoveHyphensCmd <- R6::R6Class(
  classname = "RemoveHyphensCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveHyphensCmd"
      private$..methodName <- "initialize"
      private$..meta$core$name <- private$..className
      private$logR  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveHyphens$new(x)$execute()
      return(x)
    }
  )
)
