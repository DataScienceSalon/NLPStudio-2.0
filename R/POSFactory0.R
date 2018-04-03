#------------------------------------------------------------------------------#
#                               POSFactory0                                    #
#------------------------------------------------------------------------------#
#' POSFactory0
#'
#' \code{POSFactory0}  Abstract class. Defines interface for POSFactory class.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @family POS Classes
#' @export
POSFactory0 <- R6::R6Class(
  classname = "POSFactory0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(
    ..x = character()
  ),
  public = list(

    initialize = function(x) { stop("This method is not implemented for this abstract class.") }
  )
)
