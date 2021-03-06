#------------------------------------------------------------------------------#
#                           ReplaceWordElongationCmd                           #
#------------------------------------------------------------------------------#
#' ReplaceWordElongationCmd
#'
#' \code{ReplaceWordElongationCmd} Command for the ReplaceWordElongation class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceWordElongation
#' class
#'
#' @usage ReplaceWordElongationCmd$new(impartMeaning = TRUE)
#'
#' @template textStudioParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceWordElongationCmd <- R6::R6Class(
  classname = "ReplaceWordElongationCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..impartMeaning = logical()
  ),

  public = list(
    initialize = function(impartMeaning = FALSE) {
      private$loadDependencies(name = 'ReplaceWordElongationCmd')
      private$..impartMeaning <- impartMeaning
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceWordElongation$new(x, impartMeaning = private$..impartMeaning)$execute()
      return(x)
    }
  )
)
