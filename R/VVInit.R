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

    validateQ = function(object, x) {
      status <- list()
      status[['code']] <- TRUE

      if (!("corpus" %in% class(x)[1])) {
        status$code <- FALSE
        status$msg <- paste0("Invalid object. Must be a Quanteda 'corpus' object.")
      }

      return(status)
    },

    validateDir = function(object, x) {

      status <- list()
      status[['code']] <- TRUE

      if (class(x)[1] == 'character') {

        if (isDirectory(x)) {
          files <- list.files(x, full.names = TRUE)
        } else {
          glob <- basename(x)
          dir <- dirname(x)
          files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
        }

        if (is.null(files) | length(files) == 0) {
          status$code <- FALSE
          status$msg <- paste0("No files match the criteria entered.")
        }
      } else {
        status$code <- FALSE
        status$msg <- paste0("Parameter must be class character and indicate ",
                             "a directory or wildcard string.")
      }
      return(status)
    },

    validateVector = function(object, x) {
      status <- list()
      status[['code']] <- TRUE

      if (!("character" %in% class(x)[1])) {
        if ("list" %in% class(x)[1]) {
          classes <- unique(sapply(x, function(i) {class(i)[1]}))
          if (length(classes) > 1) {
            status$code <- FALSE
            status$msg <- paste0("List must contain only character vectors.")
            return(status)
          } else if (sum("character" %in% classes) != length(classes)) {
            status$code <- FALSE
            status$msg <- paste0("List must contain only character vectors.")
            return(status)
          }
        } else {
          status$code <- FALSE
          status$msg <- paste0("Parameter must be a character vector ",
                               "or a list of character vectors.")
          return(status)
        }
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
    },

    csourceVector = function(object) {
      p <- object$getParams()
      return(private$validateVector(object, p$x))
    },

    csourceDir = function(object) {
      p <- object$getParams()
      return(private$validateDir(object, p$x))
    },

    csourceQuanteda = function(object) {
      p <- object$getParams()
      return(private$validateQ(object, p$x))
    }
  )
)
