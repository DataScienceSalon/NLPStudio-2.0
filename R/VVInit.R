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

    validateTextStudio0 = function(object, classes) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      # Validate class of object
      if (!(class(p$x)[1] %in% classes)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid class. Cannot process ",
                                  class(p$x)[1], " object. ",
                                  class(object)[1], " class operates on ",
                                  "Corpus and Document classes only. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }

      # Validate pattern replace
      replacement <- character()
      if (length(p$pattern) > 0) {
        if (is.data.frame(p$pattern)) {
          if (ncol(p$pattern) == 2) {
            pattern <- as.character(p$pattern[,1])
            replacement <- as.character(p$pattern[,2])
          } else {
            pattern <- as.character(p$pattern[,1])
            replacement <- as.character(p$replacement)
          }
        } else {
          pattern <- as.character(p$pattern)
          replacement <- as.character(p$replacement)
        }
      }

      if (length(replacement) > 0) {
        if ((length(replacement) != 1) &
            (length(replacement) != length(pattern))) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Replacement values must be of length one",
                                    ifelse(length(pattern) == 1,"",
                                           paste0(" or of length ", length(pattern))),
                                    ". See ?", class(object)[1],
                                    " for further assistance")
          return(status)
        }
      }

      # Validate Logical variables
      if (length(p$logicals$variables) > 0) {
        for (i in 1:length(p$logicals$variables)) {
          if (!(p$logicals$values[i]) %in% (c('TRUE', 'FALSE', 1, 0))) {
            status[['code']] <- FALSE
            status[['msg']] <- paste0("TRUE/FALSE expected for the '",
                                      p$logicals$variables[i],  "' variable. ",
                                      "See ?", class(object)[1],
                                      " for further assistance.")
            return(status)
          }
        }
      }

      # Validate discrete parameters
      if (length(p$discrete$variables) > 0) {
        for (i in 1:length(p$discrete$variables)) {
          if (!(p$discrete$values[i] %in% p$discrete$valid[[i]])) {
            status[['code']] <- FALSE
            status[['msg']] <- paste0("Invalid '", p$discrete$variables[i], "' parameter. ",
                                      "Valid values are ",
                                      paste0("c(",gsub(",$", "",
                                                       paste0("'",p$discrete$valid[[i]],
                                                              "',", collapse = "")),
                                             "). "),
                                      "See ?", class(object)[1],
                                      " for further assistance.")
            return(status)
          }
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

    textStudio0 = function(object) {
      return(private$validateTextStudio0(object, classes = c("Corpus", "Document")))
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
