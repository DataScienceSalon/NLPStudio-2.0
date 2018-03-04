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

    ..name = "VVDetach",

    validate = function(object, classes) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (class(p$x)[1] == "character") {
        if (is.null(p$cls)) {
          status$code <- FALSE
          status$msg <- paste0("Unable to detach ", x, ". The class of the object ",
                               "must be provided in conjunction with its name when ",
                               "the object is not given as the parameter. ",
                               "See ?", class(object)[1], " for further assistance.")
        } else if (sum(class(p$cls) %in% classes) == 0) {
          status$code <- FALSE
          status$msg <- paste0("Unable to detach ", x, ". Invalid object class.",
                               "See ?", class(object)[1], " for further assistance.")
        }
      } else if (sum(class(p$x) %in% classes) == 0) {
        status$code <- FALSE
        status$msg <- paste0("Unable to detach ", x, ". Invalid object class.",
                             "See ?", class(object)[1], " for further assistance.")
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
      classes <- c("Text")
      return(private$validate(object = object, classes = classes))
    }
  )
)
