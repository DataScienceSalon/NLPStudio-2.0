#' VVAttach
#'
#'
#' \code{VVAttach} Visitor class responsible for validating attach operations.
#'
#' Class contains the methods for validating attach requests  originating from
#' 'parent' classes.
#'
#' @usage visitor <- VVAttach$new()
#'
#' @section Core Methods:
#' The VVAttach methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the attach/detach methods parameters of the NLPStudio object.}
#'   \item{\code{corpus(object)}}{Method for validating the attach/detach methods parameters of the Corpus object}
#'   \item{\code{document(object)}}{Method for validating the attach/detach methods parameters of the Document object}
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
#' visitor <- VVAttach$new()
#' object$accept(visitor)
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVAttach <- R6::R6Class(
  classname = "VVAttach",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..composites = c("Corpus", "Document"),

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

      # Confirm class of child
      if (sum(class(p$x) %in% classes) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to attach ", class(p$x)[1],
                                  " class object to an object of class ",
                                  class(object)[1], ".",
                                 "See ?", class(object)[1],
                                 " for further assistance.")
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
      classes <- c("Document", "Corpus")
      return(private$validate(object = object, classes = classes))
    }
  )
)
