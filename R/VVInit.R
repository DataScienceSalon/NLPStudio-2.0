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

    validateName = function(object, name) {

      status <- list()
      status[['code']] <- TRUE

      v <- VString$new()
      if (v$validate(value = name) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object. ", "Object name, must be a character ",
                                  "string with no spaces.  See ?", class(object)[1],
                                  " for further assistance")
      }
      return(status)
    },

    validateClass = function(object, param, classes) {

      status <- list()
      status[['code']] <- TRUE

      if (sum(class(param) %in% classes) == 0) {
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
      return(private$validateName(object))
    },
    document = function(object) {
      p <- object$getParams()
      return(private$validateName(object, p$name))
    },
    text = function(object) {
      p <- object$getParams()
      nameVal <- private$validateName(object, p$name)
      if (nameVal$code == FALSE)  return(nameVal)
      return(private$validateClass(object, p$content, "character"))
    }
  )
)
