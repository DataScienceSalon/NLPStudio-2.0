#' Tokens
#'
#' \code{Tokens} Class for creating, managing, reading and writing
#' Tokens objects.
#'
#' Tokens objects are Corpus level objects containing collections of
#' TokenDocument objects which have been tokenized via the Tokenize class.
#'
#' @usage tokenizedCorpus <- Tokens$new(name = 'Sentence Tokens', what = "sentence")
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Tokens class.}
#'  }
#' @template entityMethods
#' @template entityParams
#' @param x The Corpus object which was tokenized.
#' @param what Character string indicating the level of tokenization. Valid
#' values are c("sentence", "word", "character").
#' @template metaParams
#'
#' @return Tokens object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @family Tokens Classes
#' @export
Tokens <- R6::R6Class(
  classname = "Tokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataCollection0,

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL, corpusId = NULL, what = c('word')) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$discrete$variables <- c('what')
      private$..params$discrete$values <- c(what)
      private$..params$discrete$valid <- list(c('sentence', 'word','character'))
      if (private$validate()$code == FALSE) stop()

      private$..what <- what
      private$coreMeta(name = name,
                       type = paste0(proper(what), " Tokens"),
                       corpusId = corpusId)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                        Document Metadata Method                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, values = NULL) {

      if (is.null(key)) {
        dm <- rbindlist(lapply(private$..attachments[['TokensDocument']], function(a) {
            a$metadata()$object
          }))
        return(dm)
      } else if (length(key) != 1) {
        event <- paste0("Key parameter must be of length one. ",
                        "See ?", class(self)[1], " for further ",
                        "assistance.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      } else  if (length(values) != 1) {
        if (length(values) != length(private$..attachments[['TokensDocument']])) {
          event <- paste0("Unable to add metadata. The values ",
                          "parameter must be of length one or ",
                          "length equal to that number of documents ",
                          "in the Tokens object. ",
                          "See ?", class(self)[1], " for further ",
                          "assistance.")
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        }
      } else {
        values <- rep(values, length(private$..attachments[['TokensDocument']]))
      }


      for (i in 1:length(private$..attachments[['TokensDocument']])) {
        private$..attachments[['TokensDocument']][[i]] <-
          private$..attachments[['TokensDocument']][[i]]$metadata(key = key, value = values[i])
      }

      event <- paste0("Updated TokensDocument' metadata.")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, stats = TRUE, system = TRUE, state = TRUE,
                       quiet = FALSE, abbreviated = FALSE, attachments = TRUE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta, quiet = quiet)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta = meta, quiet = quiet)
          section <- c("Core Metadata")
        }

        if (attachments) {
          result$attachments <- private$attachments(quiet = quiet)
          section <- c(section, "Attachments")
        }

        if (state) {
          result$app <- private$state(meta = meta, quiet = quiet)
          section <- c(section, "State Information")
        }

        if (system) {
          result$sys <- private$system(meta = meta, quiet = quiet)
          section <- c(section, "System Information")
        }

        names(result) <- section
      }
      invisible(result)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokensCollection(self)
    }
  )
)
