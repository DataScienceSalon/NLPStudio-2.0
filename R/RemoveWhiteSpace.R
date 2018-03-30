#------------------------------------------------------------------------------#
#                            Remove Extra White Space                          #
#------------------------------------------------------------------------------#
#' RemoveWhiteSpace
#'
#' \code{RemoveWhiteSpace} Removes extra white space from text.
#'
#' @usage RemoveWhiteSpace$new(x)$execute()
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
RemoveWhiteSpace <- R6::R6Class(
  classname = "RemoveWhiteSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveWhiteSpace"
      private$..methodName <- "initialize"
      private$..meta$core$name <- private$..className
      private$logR  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..regex <- "\\s+"
      private$..replacement <- " "

      invisible(self)
    }
  )
)
