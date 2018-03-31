#' Tokens
#'
#' \code{Tokens} Class for creating, managing, reading and writing Tokens objects.
#'
#' Tokens objects are Corpus level objects containing collections of Document
#' objects which have been tokenized using the Tokenize class.
#'
#' @usage tokenizedCorpus <- Tokens$new(x = corpus, to = "sentence")
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Tokens class.}
#'  }
#' @template entityMethods
#' @template entityParams
#' @param x A Corpus class object to be tokenized
#' @param to Character string indicating the level of tokenization. Valid
#' values are c("sentence", "character"), The first letter of the level
#' of tokenization may also be used in lieu of the word.
#' @template metaParams
#'
#' @return Tokens object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @export
Tokens <- R6::R6Class(
  classname = "Tokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection,

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {
      private$loadDependencies(name = name)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                        Document Metadata Method                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, values = NULL) {

      if (is.null(key)) {
        dm <- rbindlist(lapply(private$..attachments[['Tokens']], function(a) {
            a$meta()$object
          }))
        return(dm)
      } else if (length(key) != 1) {
        event <- paste0("Key parameter must be of length one. ",
                        "See ?", class(self)[1], " for further ",
                        "assistance.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      } else  if (length(values) != 1) {
        if (length(values) != length(private$..attachments[['Tokens']])) {
          event <- paste0("Unable to add metadata. The values ",
                          "parameter must be of length one or ",
                          "length equal to that number of documents ",
                          "in the Tokens object.")
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        }
      } else {
        values <- rep(values, length(private$..attachments[['Tokens']]))
      }


      for (i in 1:length(private$..attachments[['Document']])) {
        private$..attachments[['Document']][[i]] <-
          private$..attachments[['Document']][[i]]$metadata(key = key, value = values[i])
      }

      event <- paste0("Updated document metadata.")
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
          section <- c("Additional Core Metadata")
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
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function() {
      private$..methodName <- 'read'
      if (!is.null(private$..attachments[['Document']])) {
        content <- lapply(private$..attachments[['Document']], function(a) {
          a$content
        })
      }
      return(content)
    },

    write = function(path, fileNames = NULL) {
      private$..methodName <- 'write'

      if (!is.null(private$..attachments[['Document']])) {

        if (is.null(fileNames)) {
          path <- file.path(path, paste0(sapply(private$..attachments[['Document']], function(a) {
            tools::file_path_sans_ext(a$getName())
            }),".txt"))
        } else {
          if (length(fileNames) != length(private$..attachments[['Document']])) {
            event <- paste0("Unable to write the Tokens object. The ",
                                       "fileNames parameter must be NULL or have",
                                       "length = ", length(private$..attachments[['Document']]), ".")
            private$logR$log(cls = class(self)[1], event = event, level = "Error")
            stop()
          } else {
            path <- file.path(path,(paste0(tools::file_path_sans_ext(fileNames), ".txt")))
          }
        }
        lapply(seq_along(private$..attachments[['Document']]), function(x) {
            private$..attachments[['Document']][[x]]$write(path = path[x])
          })

      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
    }
  )
)
