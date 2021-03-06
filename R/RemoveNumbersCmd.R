#------------------------------------------------------------------------------#
#                            RemoveNumbersCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveNumbersCmd
#'
#' \code{RemoveNumbersCmd} Command for the RemoveNumbers class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNumbers
#' class
#'
#' @usage RemoveNumbersCmd$new()
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
RemoveNumbersCmd <- R6::R6Class(
  classname = "RemoveNumbersCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies(name = 'RemoveNumbersCmd')
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNumbers$new(x)$execute()
      return(x)
    }
  )
)
