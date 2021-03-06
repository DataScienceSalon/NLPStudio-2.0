#------------------------------------------------------------------------------#
#                             Replace Kern                                     #
#------------------------------------------------------------------------------#
#' ReplaceKern
#'
#' \code{ReplaceKern}  Replace Kern
#'
#' A wrapper for \code{\link[textclean]{replace_kern}}
#' replaces kern. In typography kerning is the adjustment of spacing. Often,
#' in informal writing, adding manual spaces (a form of kerning) coupled
#' with all capital letters is used for emphasis. This tool looks for 3 or
#' more consecutive capital letters with spaces in between and removes the spaces.
#' Essentially, the capitalized, kerned version is replaced with the word equivalent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceKern$new(x)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceKern} Returns a vector with kern replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceKern <- R6::R6Class(
  classname = "ReplaceKern",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    processDocument = function(document) {
      document$content <- textclean::replace_kern(x = document$content)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x

      invisible(self)
    }
  )
)
