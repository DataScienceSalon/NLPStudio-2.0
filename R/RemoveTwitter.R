#------------------------------------------------------------------------------#
#                              Remove Twitter                                  #
#------------------------------------------------------------------------------#
#' RemoveTwitter
#'
#' \code{RemoveTwitter} Removes twitter.
#'
#' Removes twitter handles from text.
#'
#' @usage RemoveTwitter$new(x)$execute()
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
RemoveTwitter <- R6::R6Class(
  classname = "RemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..regex <- '\\B#\\w*[a-zA-Z]+\\w*'
      private$..replacement <- " "

      invisible(self)
    }
  )
)
