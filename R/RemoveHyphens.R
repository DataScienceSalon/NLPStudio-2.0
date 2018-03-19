#------------------------------------------------------------------------------#
#                              Remove Hyphens                                  #
#------------------------------------------------------------------------------#
#' RemoveHyphens
#'
#' \code{RemoveHyphens} Removes hyphens from text.
#'
#' @usage RemoveHyphens$new(x)$execute()
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
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveHyphens"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..regex <- '[-]'
      private$..replacement <- " "

      invisible(self)
    }
  )
)
