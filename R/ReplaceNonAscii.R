#------------------------------------------------------------------------------#
#                             Replace NonAscii                                 #
#------------------------------------------------------------------------------#
#' ReplaceNonAscii
#'
#' \code{ReplaceNonAscii}  Replace Common Non-ASCII Characters.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces common non-ascii characters.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNonAscii$new(x)$execute()
#' @usage ReplaceNonAscii$new(x, removeNonConverted = FALSE)$execute()
#'
#' @template textStudioParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNonAscii} Returns a vector with non-ascii characters replaced
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNonAscii <- R6::R6Class(
  classname = "ReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..removeNonConverted = logical(),

    processDocument = function(document) {

      document$content <- textclean::replace_non_ascii(x = document$content,
                                                       remove.nonconverted =
                                                         private$..removeNonConverted)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, removeNonConverted = TRUE) {
      private$..className <- "ReplaceNonAscii"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c('removeNonConverted')
      private$..params$logicals$values <- c(removeNonConverted)
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..removeNonConverted <- removeNonConverted

      invisible(self)
    }
  )
)
