#------------------------------------------------------------------------------#
#                              Remove Numbers                                  #
#------------------------------------------------------------------------------#
#' RemoveNumbers
#'
#' \code{RemoveNumbers} Removes numbers from text.
#'
#' @usage RemoveNumbers$new(x)$execute()
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
RemoveNumbers <- R6::R6Class(
  classname = "RemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..regex <- '[[:digit:]]'
      private$..replacement <- ""

      invisible(self)
    }
  )
)
