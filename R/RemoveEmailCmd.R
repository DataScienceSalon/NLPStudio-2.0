#------------------------------------------------------------------------------#
#                            RemoveEmailCmd                                    #
#------------------------------------------------------------------------------#
#' RemoveEmailCmd
#'
#' \code{RemoveEmailCmd} Command for the RemoveEmail class.
#'
#' Class that encapsulates the command to execute an object of the RemoveEmail
#' class
#'
#' @usage RemoveEmailCmd$new()
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
RemoveEmailCmd <- R6::R6Class(
  classname = "RemoveEmailCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies(name = 'RemoveEmailCmd')
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveEmail$new(x)$execute()
      return(x)
    }
  )
)
