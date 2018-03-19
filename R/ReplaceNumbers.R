#------------------------------------------------------------------------------#
#                             Replace Numbers                                  #
#------------------------------------------------------------------------------#
#' ReplaceNumbers
#'
#' \code{ReplaceNumbers}  Replace Numbers With Text Representation.
#'
#' A wrapper for \code{\link[textclean]{replace_number}} Replaces numeric represented numbers with words (e.g., 1001 becomes one thousand one).
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNumbers$new(x)$execute()
#' @usage ReplaceNumbers$new(x, joinNumbers = TRUE, remove = FALSE)$execute()
#'
#' @template textStudioParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNumbers} Returns a vector with numbers replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNumbers <- R6::R6Class(
  classname = "ReplaceNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..joinNumbers = logical(),
    ..remove = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_number(x = document$content,
                                            num.paste = private$..joinNumbers,
                                            remove = private$..remove)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, joinNumbers = FALSE, remove = FALSE) {
      private$..className <- "ReplaceNumbers"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c('joinNumbers', 'remove')
      private$..params$logicals$values <- c(joinNumbers, remove)
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove

      invisible(self)
    }
  )
)
