#------------------------------------------------------------------------------#
#                                ReshapeCmd                                    #
#------------------------------------------------------------------------------#
#' ReshapeCmd
#'
#' \code{ReshapeCmd} Command for the Reshape class.
#'
#' Class that encapsulates the command to execute an object of the Reshape
#' class
#'
#' @usage ReshapeCmd$new(to = "sentence")
#'
#' @template textStudioParams
#' @param to Character string indicating the new level of aggregation. Valid
#' levels include c(corpus, document, sentence).  The first letters of the
#' aggregation levels may also be used.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReshapeCmd <- R6::R6Class(
  classname = "ReshapeCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..to = character()
  ),

  public = list(
    initialize = function(to = "sentence") {
      private$loadDependencies(name = 'ReshapeCmd')
      private$..to <- to
      invisible(self)
    },
    execute = function(x) {
      x <- Reshape$new(x, to = private$..to)$execute()
      return(x)
    }
  )
)
