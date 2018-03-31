#------------------------------------------------------------------------------#
#                         Replace Internet Slang                               #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlang
#'
#' \code{ReplaceInternetSlang}  Replace Internet Slang
#'
#' A wrapper for \code{\link[textclean]{replace_internet_slang}}
#' replaces internet slang.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceInternetSlang$new(x, slang = NULL, replacement = NULL, ignoreCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param slang A vector of slang strings to replace.
#' @param replacement A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case).
#' Applies to default internet slang only.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceInternetSlang} Returns a vector with internet slang replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceInternetSlang <- R6::R6Class(
  classname = "ReplaceInternetSlang",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_internet_slang(x = document$content,
                                         slang = private$..slang,
                                         replacement = private$..replacement,
                                         ignore.case = private$..ignoreCase)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, slang = NULL, replacement = NULL, ignoreCase = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      private$..params$pattern <- slang
      private$..params$replacement <- replacement
      private$..params$logicals$variables <- c('ignoreCase')
      private$..params$logicals$values <- c(ignoreCase)
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..slang <- slang
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase

      invisible(self)
    }
  )
)
