#------------------------------------------------------------------------------#
#                           ReplaceCurlyQuotesCmd                              #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotesCmd
#'
#' \code{ReplaceCurlyQuotesCmd} Command for the ReplaceCurlyQuotes class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceCurlyQuotes
#' class
#'
#' @usage ReplaceCurlyQuotesCmd$new()
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
ReplaceCurlyQuotesCmd <- R6::R6Class(
  classname = "ReplaceCurlyQuotesCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies(name = 'ReplaceCurlyQuotesCmd')
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceCurlyQuotes$new(x)$execute()
      return(x)
    }
  )
)
