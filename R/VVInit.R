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

    validateSplits = function(object) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      # Validate class of parameter
      if ("SplitCorpus" %in% class(object)[1]) {
        if (!("Corpus" %in%  class(p$x)[1])) {
          status$code <- FALSE
          status$msg <- paste0("Object must be of the Corpus class. ",
                               "See ?", class(object)[1], " for further assistance.")
          return(status)
        }
      } else if ("SplitDocument" %in% class(object)[1]) {
        if (!("Document" %in%  class(p$x)[1])) {
          status$code <- FALSE
          status$msg <- paste0("Object must be of the Document class. ",
                               "See ?", class(object)[1], " for further assistance.")
          return(status)
        }
      } else {
        status$code <- FALSE
        status$msg <- paste0("This method is for validating SplitCorpus and ",
                             "SplitDocument class instantiation.",
                             "See ?", class(object)[1], " for further assistance.")
        return(status)
      }


      # Validate splits add to 1
      if (sum(p$splits) != 1) {
        status$code <- FALSE
        status$msg <- paste0("Splits must add to one.")
        return(status)
      }
      if (!is.null(p$setNames)) {
        if (length(p$setNames) != length(p$splits)) {
          status$code <- FALSE
          status$msg <- paste0("The setNames vector must be of length zero ",
                               " or length equal to that of the splits vector.
                               See ?", class(object)[1], " for further assistance.")
          return(status)
        }
      }
      return(status)
    },

    validateTM = function(object) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()
      classes <- c("VCorpus", "Corpus", "SimpleCorpus", "PCorpus")

      if (!(class(p$x)[1] %in% classes)) {
        status$code <- FALSE
        status$msg <- paste0("Invalid object. Must be a tm package 'VCorpus', ",
                             "'Corpus','SimpleCorpus' or 'PCorpus' object.")
      }

      return(status)
    },

    validateQ = function(object, x) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!("corpus" %in% class(p$x)[1])) {
        status$code <- FALSE
        status$msg <- paste0("Invalid object. Must be a Quanteda 'corpus' object.")
      }

      return(status)
    },

    validateDir = function(object) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (class(p$x)[1] == 'character') {

        if (isDirectory(p$x)) {
          files <- list.files(p$x, full.names = TRUE)
        } else {
          glob <- basename(p$x)
          dir <- dirname(p$x)
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

    validateVector = function(object) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!("character" %in% class(p$x)[1])) {
        if ("list" %in% class(p$x)[1]) {
          classes <- unique(sapply(p$x, function(i) {class(i)[1]}))
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

    validateFile = function(object) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!(R.utils::isFile(p$x))) {
        status$code <- FALSE
        status$msg <- paste0("File ", x, " does not exist.",
                             "See ?", class(object)[1],
                             " for further assistance")
      }
      return(status)
    },

    validateClass = function(object, classes) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!is.null(p$x)) {

        if  (!(class(p$x)[1] %in% classes)) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Invalid class. Cannot create ",
                                    class(object)[1],
                                    " object. ", "Parameter is not of a valid class. ",
                                    "See ?", class(object)[1],
                                    " for further assistance")
        }
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
      return(private$validateClass(object, classes = "character"))
    },

    #-------------------------------------------------------------------------#
    #                   Validate Corpus Source Classes                        #
    #-------------------------------------------------------------------------#

    csourceVector = function(object) {
      return(private$validateVector(object))
    },

    csourceDir = function(object) {
      return(private$validateDir(object))
    },

    csourceQuanteda = function(object) {
      return(private$validateQ(object))
    },

    csourceTM = function(object) {
      return(private$validateTM(object))
    },

    #-------------------------------------------------------------------------#
    #                 Validate Text Processing Classes                        #
    #-------------------------------------------------------------------------#
    splitCorpus = function(object) {
      return(private$validateSplits(object))
    },

    splitDocument = function(object) {
      return(private$validateSplits(object))
    },

    textStudio = function(object) {
      return(private$validateClass(object, classes = c("Corpus", "Document")))
    },

    #-------------------------------------------------------------------------#
    #                             Misc Classes                                #
    #-------------------------------------------------------------------------#
    klone = function(object) {
      return(private$validateClass(object, classes = c("Corpus", "Document")))
    },

    cloneCorpus = function(object) {
      return(private$validateClass(object, classes = c("Corpus")))
    },

    cloneDocument = function(object) {
      return(private$validateClass(object, classes = c("Document")))
    }
  )
)
