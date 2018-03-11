#' VVInit
#'
#' \code{VVInit} Visitor class responsible for validating the initialization of objects.
#'
#' \strong{VVInit Methods:}
#' The VVInit methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the instantiation of the NLPStudio object}
#' }
#'
#' @param object The object in its current state
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVInit <- R6::R6Class(
  classname = "VVInit",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validateDir = function(object, x) {

      status <- list()
      status[['code']] <- TRUE

      if (!(R.utils::isDirectory(x))) {
        status$code <- FALSE
        status$msg <- paste0("Directory ", x, " does not exist.",
                             "See ?", class(object)[1],
                             " for further assistance")
      }
      return(status)
    },

    validateFile = function(object, x) {

      status <- list()
      status[['code']] <- TRUE

      if (!(R.utils::isFile(x))) {
        status$code <- FALSE
        status$msg <- paste0("File ", x, " does not exist.",
                             "See ?", class(object)[1],
                             " for further assistance")
      }
      return(status)
    },

    validateClass = function(object, param, classes) {

      status <- list()
      status[['code']] <- TRUE

      if (!is.null(param) & sum(class(param) %in% classes) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid class. Cannot create ",
                                  class(object)[1],
                                  " object. ", "Parameter is not of a valid class. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
      }
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Validate Core Classes                            #
    #-------------------------------------------------------------------------#
    nlpStudio = function(object) {
      return(status[['code']] <- TRUE)
    },
    corpus = function(object) {
      return(status[['code']] <- TRUE)

    },
    document = function(object) {
      return(status[['code']] <- TRUE)
    },
    text = function(object) {
      p <- object$getParams()
      return(private$validateClass(object, p$x, classes = "character"))
    }
  )
)
