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
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      # Initiate logging variables and system meta data
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Complete Initialization
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                    Document Metadata Method                             #
    #-------------------------------------------------------------------------#
    docMeta = function(key, values) {

      private$..methodName <- 'docMeta'

      if (length(values) != 1) {
        if (length(values) != length(private$..attachments[['Document']])) {
          private$..event <- paste0("Unable to add metadata. The values ",
                                     "parameter must be of length one or ",
                                     "length equal to that number of documents ",
                                     "in the Corpus object.")
          private$logIt("Error")
          stop()
        }
      } else {
        values <- rep(values, length(private$..attachments[['Document']]))
      }

      for (i in 1:length(private$..attachments[['Document']])) {
        private$..attachments[[Document]][[i]] <-
          private$..attachments[['Document']][[i]]$meta(key = key, value = values[i])
        print(private$..attachments[['Document']][[i]]$meta())
      }

      private$..event <- paste0("Updated document metadata")
      private$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function() {
      private$..methodName <- 'read'
      content <- lapply(private$..attachments, function(a) {
        a$content
      })
      return(content)
    },

    write = function(path, fileNames = NULL) {
      private$..methodName <- 'write'

      if (is.null(fileNames)) {
        path <- file.path(path, paste0(sapply(private$..attachments, function(a) {
          tools::file_path_sans_ext(a$getName())
          }),".txt"))
      } else {
        if (length(fileNames) != length(private$..attachments)) {
          private$..event <- paste0("Unable to write the Corpus object. The ",
                                     "fileNames parameter must be NULL or have",
                                     "length = ", length(private$..attachments), ".")
          private$logIt("Error")
          stop()
        } else {
          path <- file.path(path,(paste0(tools::file_path_sans_ext(fileNames), ".txt")))
        }
      }
      lapply(seq_along(private$..attachments), function(x) {
          private$..attachments[[x]]$write(path = path[x])
        })
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
