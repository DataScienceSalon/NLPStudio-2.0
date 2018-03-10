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

    validate = function(object) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()
      key <- p$key
      value <- p$value

      if ((is.null(key) & !is.null(value)) |
          (!is.null(key) & is.null(value))) {
            status$code <- FALSE
            status$msg <- paste0("Both key & value, must be both null or ",
                                 "they both must be non-null. See ?",
                                 class(object)[1], " for further information.")
      } else if (!is.null(key) & !is.null(value)) {
        if (class(key) != 'character') {
          status$code <- FALSE
          status$msg <- paste0("Key must of the character class See ?",
                               class(object)[1], " for further information.")
        }
      }
      return(status)
    }
  ),
  public = list(

    initialize = function() { invisible(self) },

    nlpStudio = function(object) {
      classes <- c("Pipeline", "Corpus")
      return(private$validate(object = object))
    },

    pipeline = function(object) {
      classes <- "Corpus"
      return(private$validate(object = object))
    },

    corpus = function(object) {
      classes <- c("Document")
      return(private$validate(object = object))
    },

    document = function(object) {
      return(private$validate(object = object))
    }
  )
)
