#------------------------------------------------------------------------------#
#                             Replace Names                                    #
#------------------------------------------------------------------------------#
#' ReplaceNames
#'
#' \code{ReplaceNames}  Replaces first/last names.
#'
#' A wrapper for \code{\link[textclean]{replace_names}}
#' Replaces first and last names.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNames$new(x, names = NULL, replacement = NULL)$execute()
#'
#' @template textStudioParams
#' @param names Vector of names to replace.
#' @param replacement A string with which to replace names.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNames} Returns a vector with names replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNames <- R6::R6Class(
  classname = "ReplaceNames",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..names = character(),

    processDocument = function(document) {
      document$content <- textclean::replace_names(x = document$content,
                                          names = private$..meta$object$name,
                                          replacement = private$..replacement)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, names = NULL, replacement = NULL) {
      private$..className <- "ReplaceNames"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      private$..x <- x
      private$..meta$object$name <- names
      private$..replacement <- replacement

      invisible(self)
    }
  )
)
