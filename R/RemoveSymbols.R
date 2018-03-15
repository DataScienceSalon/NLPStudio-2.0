#------------------------------------------------------------------------------#
#                              Remove Symbols                                  #
#------------------------------------------------------------------------------#
#' RemoveSymbols
#'
#' \code{RemoveSymbols} Removes symbols.
#'
#' Removes symbols (all non-alphanumeric characters) from text.
#'
#' @usage RemoveSymbols$new(x)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveSymbols <- R6::R6Class(
  classname = "RemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveSymbols"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveSymbols"
      private$..x <- x
      private$..regex <- "[^[:alnum:]]"
      private$..replacement <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)