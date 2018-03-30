#' Corpus
#'
#' \code{Corpus} Class for creating, managing, reading and writing Corpus objects.
#'
#' Corpus objects are collections of Document objects and the primary level
#' of document aggregation at which document processing, feature engineering,
#' selection, analysis and modeling occurs.
#'
#' @usage myCorpus <- Corpus$new(name = "machineLearning", content = mlAdvances)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Corpus class.}
#'  }
#' @template entityMethods
#'
#' @template entityParams
#' @param x A Document class object, or a list thereof, to be attached to the Corpus object
#' @template metaParams
#'
#' @return Corpus object.
#'
#' @examples
#'
#' # Instantiate
#' corpus <- Corpus$new(name = 'corpus')
#' corpus$summary()
#'
#' # Attach / Detach Document objects
#'
#' ## Create Text objects
#' blogsTxt <- Text$new(name = 'blogs (raw)', x = "./data/en_US.blogs.txt")
#' newsTxt <- Text$new(name = 'news (raw)', x = "./data/en_US.news.txt")
#' twitterTxt <- Text$new(name = 'twitter (raw)', x = "./data/en_US.twitter.txt")
#'
#' ## Create Document objects and attach Texts
#' blogsDoc <- Document$new()$attach(blogsTxt)
#' newsDoc <- Document$new()$attach(newsTxt)
#' twitterDoc <- Document$new()$attach(twitterTxt)
#'
#' ## Attach Document objects to Corpus
#' corpus$attach(x = list(blogsDoc, newsDoc, twitterDoc))
#'
#' ## Detach Document object from Corpus
#' corpus$detach(key = 'name', value = 'blogsDoc')
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection,

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {
      private$loadDependencies(name = name)
      private$initMeta(name = name)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Document Management Methods                       #
    #-------------------------------------------------------------------------#
    getDocument = function(key = NULL, value = NULL) {
      objects <- private$get(cls = "Document", key, value)
      return(objects)
    },

    addDocument = function(x) {
      private$attach(x)
      invisible(self)
    },

    removeDocument = function(x) {
      private$detach(x)
      return(self)
    },


    #-------------------------------------------------------------------------#
    #                              Data Methods                               #
    #-------------------------------------------------------------------------#
    dfm = function() {
      dfm <- private$get(cls = "DFM")
      return(objects)
    },

    removeDFM = function(x) {
      private$detach(x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Document Metadata Method                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, values = NULL) {

      if (is.null(key)) {
        dm <- rbindlist(lapply(private$..attachments[['Document']], function(a) {
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
        if (length(values) != length(private$..attachments[['Document']])) {
          event <- paste0("Unable to add metadata. The values ",
                          "parameter must be of length one or ",
                          "length equal to that number of documents ",
                          "in the Corpus object.")
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        }
      } else {
        values <- rep(values, length(private$..attachments[['Document']]))
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
            event <- paste0("Unable to write the Corpus object. The ",
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
