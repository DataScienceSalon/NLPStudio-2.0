#' VVMetadata
#'
#' \code{VVMetadata} Visitor class responsible for validating calls to metadata methods.
#'
#' \strong{VVMetadata Methods:}
#' The VVMetadata methods are as follows:
#'  \itemize{
#'   \item{\code{corpus(object)}}{Method for validating calls to metadata from Corpus objects.}
#'   \item{\code{document(object)}}{Method for validating calls  to metadata from Document objects.}
#' }
#'
#' @param object The object in its current state
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVMetadata <- R6::R6Class(
  classname = "VVMetadata",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    initialize = function() {
      invisible(self)
    },

    corpus = function(object) {
      return(validateKeyValue(object))
    },

    textDocument = function(object) {
      return(validateKeyValue(object))
    }
  )
)
