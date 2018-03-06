#' VVGet
#'
#' \code{VVGet} Visitor class responsible for validating get methods
#'
#' Validates get methods, the methods used to obtain information regarding an object
#'
#' @usage visitor <- VVGet$new()
#'
#' @section Core Methods:
#' The VVGet methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the attach/detach methods parameters of the NLPStudio object.}
#'   \item{\code{corpus(object)}}{Method for validating the attach/detach methods parameters of the Corpus object}
#'   \item{\code{document(object)}}{Method for validating the attach/detach methods parameters of the Document object}
#' }
#'
#' @param object The domain object
#' @return status List containing:
#' \itemize{
#'  \item status Logical TRUE, if validation was passed, FALSE otherwise
#'  \item msg Character string describing the error, if status is FALSE
#' }
#'
#' @examples
#' visitor <- VVGet$new()
#' object$accept(visitor)
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVGet <- R6::R6Class(
  classname = "VVGet",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object, classes) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()
      x <- p$x
      value <- p$value

      if (!is.null(x)) {
        if (sum(class(x) %in% classes) == 0) {
          if (is.null(value)) {
            status$code <- FALSE
            status$msg <- paste0("Value of key/value pair not provided in ",
                                 "conjunction with the x parameter.")
          }
        }
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
