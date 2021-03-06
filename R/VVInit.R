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

    validateStudio = function(object, classes = NULL) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      # Validate class of object
      if (!is.null(p$x)) {
        if (!(class(p$x)[1] %in% classes)) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Invalid class. Cannot process ",
                                    class(p$x)[1], " object. ",
                                    class(object)[1], " class operates on ",
                                    paste0("c(",gsub(",$", "",
                                                     paste0("'",classes,
                                                            "',", collapse = "")),
                                           ") "),
                                    " classes only. ",
                                    "See ?", class(object)[1],
                                    " for further assistance")
          return(status)
        }
      }
      # Validate pattern replace
      if (!is.null(p$pattern)) {
        status <- validateKeyValue(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate Logicals
      if (!is.null(p$logicals)) {
        status <- validateLogical(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate discrete parameters
      if (!is.null(p$discrete)) {
        status <- validateDiscrete(object)
        if (status$code == FALSE) {
          return(status)
        }
      }
      return(status)
    },

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
    textDocument = function(object) {
      return(private$validateClass(object, classes = c("character")))
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

    #-------------------------------------------------------------------------#
    #                 Validate Data Processing Classes                        #
    #-------------------------------------------------------------------------#
    tokenize = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    tokensDocument = function(object) {
      return(private$validateStudio(object, classes = c('character', 'tokens')))
    },

    tokensCollection = function(object) {
      return(private$validateStudio(object))
    },

    posFactory = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    posDocument = function(object) {
      return(private$validateStudio(object, classes = c('character')))
    },

    posCollection = function(object) {
      return(private$validateStudio(object))
    },

    termFreq = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    termFreqFactoryDfm = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    termFreqFactoryDtm = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    termFreqFactoryTdm = function(object) {
      return(private$validateStudio(object, classes = c('Corpus')))
    },

    termFreqDfm = function(object) {
      return(private$validateStudio(object, classes = c('dfm')))
    },

    termFreqDtm = function(object) {
      return(private$validateStudio(object, classes = c('DocumentTermMatrix')))
    },

    termFreqTdm = function(object) {
      return(private$validateStudio(object, classes = c('TermDocumentMatrix')))
    },

    #-------------------------------------------------------------------------#
    #                             Studio Classes                              #
    #-------------------------------------------------------------------------#
    textStudio = function(object) {
      return(private$validateClass(object, classes = c("Corpus", "TextDocument")))
    },

    dataStudio = function(object) {
      return(private$validateClass(object, classes = c("Corpus")))
    },


    #-------------------------------------------------------------------------#
    #                             Misc Classes                                #
    #-------------------------------------------------------------------------#
    klone = function(object) {
      return(private$validateClass(object, classes = c("Corpus", "TextDocument")))
    },

    cloneCorpus = function(object) {
      return(private$validateClass(object, classes = c("Corpus")))
    },

    cloneDocument = function(object) {
      return(private$validateClass(object, classes = c("TextDocument")))
    }
  )
)
