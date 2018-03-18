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

    validateTextStudio = function(object, classes) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!(class(p$x)[1] %in% classes)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid class. Cannot process ",
                                  class(p$x)[1], " object. ",
                                  "TextStudio classes operate on ",
                                  "Corpus and Document classes only. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }

      for (i in 1:length(p$variables)) {
        if (!(p$values[i] %in% p$valid[[i]])) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Invalid '", p$variables[i], "' parameter. ",
                                    "Valid values are ",
                                      paste0("c(",gsub(",$", "",
                                                       paste0("'",p$valid[[i]],
                                                              "',", collapse = "")),
                                                                "). "),
                                    "See ?", class(object)[1],
                                    " for further assistance.")
          return(status)
        }
      }
      return(status)
    },

    validateReplace = function(object) {

      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!(class(p$x)[1] %in% c("Corpus", "Document"))) {
        status$code <- FALSE
        status$msg <- paste0("Object must be of the Corpus or Document classes. ",
                             "See ?", class(object)[1], " for further assistance.")
        return(status)
      }

      if (length(p$replacement) != 1) {
        if (length(p$replacement) != length(p$pattern)) {
          status$code <- FALSE
          status$msg <- paste0("Length of the replacement parameter must be one or ",
                               length(p$pattern), " the length of the pattern vector. ",
                               "See ?", class(object)[1], " for further assistance.")
          return(status)
        }
      }

      if (!is.logical(p$logical)) {
        status$code <- FALSE
        status$msg <- paste0("Value provided where TRUE/FALSE expected. ",
                             "See ?", class(object)[1], " for further assistance.")
        return(status)
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
    document = function(object) {
      return(private$validateClass(object, classes = "character"))
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

    tokenize = function(object) {
      return(private$validateTextStudio(object, classes = c("Corpus", "Document")))
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
