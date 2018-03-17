#------------------------------------------------------------------------------#
#                           Replace Curly Quotes                               #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotes
#'
#' \code{ReplaceCurlyQuotes}  Replaces curly single and double quotes.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces curly single and double quotes.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceCurlyQuotes$new(x, removeNonCoverted)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceCurlyQuotes} Returns a vector with curly quotes replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceCurlyQuotes <- R6::R6Class(
  classname = "ReplaceCurlyQuotes",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    processDocument = function(document) {
      content <- document$content
      Encoding(document$content) <- "latin1"
      document$content <- textclean::replace_curly_quote(x = content)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceCurlyQuotes"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      private$..x <- x

      invisible(self)
    }
  )
)
