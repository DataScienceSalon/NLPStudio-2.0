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

      if (class(x)[1] == "character") {
        if (length(x) == 1) {
          if (!(R.utils::isDirectory(x))) {
            status$code <- FALSE
            status$msg <- paste0("Directory does not exist.",
                                 "See ?", class(object)[1],
                                 " for further assistance")
          }
        }
      }
      return(status)
    },

    validateFile = function(object, x, required = TRUE) {

      status <- list()
      status[['code']] <- TRUE

      if (required & is.null(x)) {
        status$code <- FALSE
        status$msg <- paste0("File does not exist.",
                             "See ?", class(object)[1],
                             " for further assistance")
      }

      if (!is.null(x) & class(x)[1] == "character") {
        if (length(x) == 1) {
          if (!(R.utils::isFile(x))) {
            status$code <- FALSE
            status$msg <- paste0("File does not exist.",
                                 "See ?", class(object)[1],
                                 " for further assistance")
          }
        }
      }
      return(status)
    },

    validateX = function(object, x, classes, directory = FALSE, required = TRUE) {

      status <- list()
      status[['code']] <- TRUE

      if (is.null(x) & required == TRUE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("x parameter missing with no default. ",
                                  "  See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      } else if (!is.null(x)) {
        classVal <- private$validateClass(object, param = x, classes = classes)
        if (classVal$code == FALSE) {
          return(classVal)
        } else if (class(x)[1] == 'character') {
          if (length(x) == 1) {
            if (directory) {
              if (!R.utils::isDirectory(x)) {
                status[['code']] <- FALSE
                status[['msg']] <- paste0("Directory does not exist. ",
                                          "  See ?", class(object)[1],
                                          " for further assistance")
                return(status)
              }
            } else if (!R.utils::isFile(x)) {
              status[['code']] <- FALSE
              status[['msg']] <- paste0("File does not exist. ",
                                        "  See ?", class(object)[1],
                                        " for further assistance")
              return(status)
            }
          }
        }
      }
      return(status)
    },


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
      p <- object$getParams()
      nameVal <- private$validateName(object, p$name)
      if (nameVal$code == FALSE)  return(nameVal)
      return(private$validateX(object, p$x,
                               classes = c("Document","character", "list"),
                               directory = TRUE, required = FALSE))

    },
    document = function(object) {
      p <- object$getParams()
      nameVal <- private$validateName(object, p$name)
      if (nameVal$code == FALSE)  return(nameVal)
      return(private$validateX(object, p$x, classes = c("Text", "character"),
                               directory = FALSE, required = FALSE))
    },
    text = function(object) {
      p <- object$getParams()
      nameVal <- private$validateName(object, p$name)
      if (nameVal$code == FALSE)  return(nameVal)
      return(private$validateX(object, p$x, classes = "character",
                               directory = FALSE, required = TRUE))
    }
  )
)
