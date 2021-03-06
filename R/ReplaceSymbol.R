#------------------------------------------------------------------------------#
#                             Replace Symbol                                   #
#------------------------------------------------------------------------------#
#' ReplaceSymbol
#'
#' \code{ReplaceSymbol}  Replace Symbols With Word Equivalents
#'
#' A wrapper for \code{\link[textclean]{replace_symbol}} This function replaces symbols
#' with word equivalents (e.g., @ becomes "at".
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceSymbol$new(x, dollar = TRUE)$execute()
#' @usage ReplaceSymbol$new(x, dollar = FALSE, percent = TRUE)$execute()
#'
#' @template textStudioParams
#' @param dollar logical. If TRUE replaces dollar sign (\$) with "dollar".
#' @param percent logical. If TRUE replaces percent sign (\%) with "percent".
#' @param pound logical. If TRUE replaces pound sign (\#) with "number".
#' @param at logical. If TRUE replaces at sign (\@) with "at".
#' @param and logical. If TRUE replaces and sign (\&) with "and".
#' @param  with logical. If TRUE replaces with sign (w/) with "with"
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceSymbol} Returns a vector with symbols replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceSymbol <- R6::R6Class(
  classname = "ReplaceSymbol",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..dollar = logical(),
    ..percent = logical(),
    ..pound = logical(),
    ..at = logical(),
    ..and = logical(),
    ..with = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_symbol(x = document$content,
                                            dollar = private$..dollar,
                                           percent = private$..percent,
                                           pound = private$..pound,
                                           at = private$..at,
                                           and = private$..and,
                                           with = private$..with)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, dollar = TRUE, percent = TRUE, pound = TRUE,
                          at = TRUE, and = TRUE, with = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c('dollar', 'percent', 'pound',
                                               'at', 'and', 'with')
      private$..params$logicals$values <- c(dollar, percent, pound, at, and, with)
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..dollar <- dollar
      private$..percent <- percent
      private$..pound <- pound
      private$..at <- at
      private$..and <- and
      private$..with <- with

      invisible(self)
    }
  )
)
