#' Validator
#'
#' Class accepts validation requests from client objects and dispatches the
#' appropriate Visitor class.
#'
#' \code{Validator} Class responsible for validation of requests pertaining to:
#' \itemize{
#'  \item Object instantiation
#'  \item Composition and Aggregation: Requests to manipulate aggregate and composite objects
#'  \item Metadata: Enforces metadata consistency
#' }
#'
#' @section Validator methods:
#' This section summarizes the methods in the Validator class.
#' \describe{
#'  \item{\code{init(object)}}{Dispatches the initialization validation visitor via the accept method of object.}
#'  \item{\code{attach(object)}}{Dispatches the attach validation visitor via the accept method of object.}
#'  \item{\code{meta(object)}}{Dispatches the meetadata validation visitor via the accept method of object.}
#'  }
#'
#' @param object Originator object
#'
#' @return A list containing:
#' \itemize{
#'  \item status: Logical. If validation passed, then, TRUE, otherwise FALSE
#'  \item msg: Character string describing the error if status is FALSE
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator <- R6::R6Class(
  "Validator",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    init = function(object) {
      visitor <- VVInit$new()
      object$accept(visitor)
    },

    attach = function(object) {
      visitor <- VVAttach$new()
      object$accept(visitor)
    },

    detach = function(object) {
      visitor <- VVDetach$new()
      object$accept(visitor)
    }
  )
)
