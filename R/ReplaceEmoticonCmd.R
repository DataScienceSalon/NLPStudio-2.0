#------------------------------------------------------------------------------#
#                           ReplaceEmoticonCmd                                 #
#------------------------------------------------------------------------------#
#' ReplaceEmoticonCmd
#'
#' \code{ReplaceEmoticonCmd} Command for the ReplaceEmoticon class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoticon
#' class
#'
#' @usage ReplaceEmoticonCmd$new(emoticons = NULL)
#'
#' @template textStudioParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoticonCmd <- R6::R6Class(
  classname = "ReplaceEmoticonCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emoticons = data.table()
  ),

  public = list(
    initialize = function(emoticons = NULL) {
      private$loadDependencies(name = 'ReplaceEmoticonCmd')
      private$..emoticons <- emoticons
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceEmoticon$new(x, private$..emoticons)$execute()
      return(x)
    }
  )
)
