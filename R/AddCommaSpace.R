#------------------------------------------------------------------------------#
#                              Add Comma Space                                 #
#------------------------------------------------------------------------------#
#' AddCommaSpace
#'
#' \code{AddCommaSpace} Adds space after comma.
#'
#' Class adds space after comma when commas are followed by non space characters.
#'
#' @usage AddCommaSpace$new(x)$execute()
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
AddCommaSpace <- R6::R6Class(
  classname = "AddCommaSpace",
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
      private$..regex <- "(,)([^ ])"
      private$..replacement <- "\\1 \\2"
      invisible(self)
    }
  )
)
