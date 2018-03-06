#' VVDetach
#'
#'
#' \code{VVDetach} Visitor class responsible for validating detach operations.
#'
#' Class contains the methods for validating detach requests  originating from
#' 'parent' classes.
#'
#' @usage visitor <- VVDetach$new()
#'
#' @section Core Methods:
#' The VVDetach methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the detach/detach methods parameters of the NLPStudio object.}
#'   \item{\code{corpus(object)}}{Method for validating the detach/detach methods parameters of the Corpus object}
#'   \item{\code{document(object)}}{Method for validating the detach/detach methods parameters of the Document object}
#' }
#'
#' @param object The parent object
#' @return status List containing:
#' \itemize{
#'  \item status Logical TRUE, if validation was passed, FALSE otherwise
#'  \item msg Character string describing the error, if status is FALSE
#' }
#'
#' @examples
#' visitor <- VVDetach$new()
#' object$accept(visitor)
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVDetach <- R6::R6Class(
  classname = "VVDetach",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..composites = c("NLPStudio", "Pipeline", "Corpus", "Document"),

    validate = function(object, classes) {
      status <- list()
      status[['code']] <- TRUE

      if (!(class(object)[1] %in% private$..composites)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("This method is not implemented for this ",
                                  "class.")
        return(status)
      }

      p <- object$getParams()

      if ((length(p$key) > 1) & (length(p$key) != length(p$value))) {
        status$code <- FALSE
        status$msg <- paste0("The key parameter must be of length equal to 1 ",
                             "or equal in length to the value vector, ",
                             "See ?", class(object)[1], " for further assistance.")
        return(status)
      }
      return(status)
    }
  ),
  public = list(

    initialize = function() { invisible(self) },

    nlpStudio = function(object) {
      classes <- c("Pipeline", "Corpus")
      return(private$validate(object = object, classes = classes))
    },

    pipeline = function(object) {
      classes <- "Corpus"
      return(private$validate(object = object, classes = classes))
    },

    corpus = function(object) {
      classes <- c("Document")
      return(private$validate(object = object, classes = classes))
    },

    document = function(object) {
      classes <- c("Text", "Data", "Analysis")
      return(private$validate(object = object, classes = classes))
    }
  )
)
