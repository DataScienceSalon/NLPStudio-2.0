#' TokensCollection
#'
#' \code{TokensCollection} Class for creating, managing, reading and writing
#' TokensCollection objects.
#'
#' TokensCollection objects are Corpus level objects containing collections of
#' TokenDocument objects which have been tokenized via the Tokenize class.
#'
#' @usage tokenizedCorpus <- TokensCollection$new(name = 'Sentence Tokens', what = "sentence")
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Tokens class.}
#'  }
#' @template entityMethods
#' @template entityParams
#' @param what Character string indicating the level of tokenization. Valid
#' values are c("sentence", "word", "character"), The first letter of the level
#' of tokenization may also be used in lieu of the word.
#' @template metaParams
#'
#' @return TokensCollection object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @family Tokens Classes
#' @export
TokensCollection <- R6::R6Class(
  classname = "TokensCollection",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection,

  private = list(
    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    core = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$core, stringsAsFactors = FALSE,
                          row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\n\nObject Id    : ", meta$core$id))
        cat(paste0("\nObject Class : ", meta$core$class))
        cat(paste0("\nObject Name  : ", meta$core$name))
        cat(paste0("\nObject Type  : ", meta$core$type))
        cat(paste0("\nDescription  : ", meta$core$description))

        otherMeta <- df %>% select(-id, -class, -type, -name, -description)
        if (ncol(otherMeta) > 0) {
          cat("\n\nAdditional Core Metadata:\n")
          print(otherMeta, row.names = FALSE)
        }
      }
      return(df)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL, what = c('word')) {

      private$loadDependencies(name = name)

      # Validation
      private$..params <- list()
      private$..params$discrete$variables <- c('what')
      private$..params$discrete$values <- c(what)
      private$..params$discrete$valid <- list(c('sentence', 'word','character', 's', 'w', 'c'))
      if (private$validate()$code == FALSE) stop()

      private$..what <- what
      private$coreMeta(name = name,
                       type = ifelse(grepl(what, "word") | grepl(what, "w"), "Word Tokens",
                                     ifelse(grepl(what, "sentence") | grepl(what, "s"), "Sentence Tokens",
                                            "Character Tokens")))
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                        Document Metadata Method                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, values = NULL) {

      if (is.null(key)) {
        dm <- rbindlist(lapply(private$..attachments[['TokensDocument']], function(a) {
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
        if (length(values) != length(private$..attachments[['TokensDocument']])) {
          event <- paste0("Unable to add metadata. The values ",
                          "parameter must be of length one or ",
                          "length equal to that number of documents ",
                          "in the TokensCollection object. ",
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
