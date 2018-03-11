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
#' @param object Document Object to be attached (or detached) to or from the Corpus object.
#' @param x A Document object, or the name thereto. Used in the detach method.
#' @template ioParams
#' @template metaParams
#'
#' @return Corpus object, containing related Document objects.
#'
#' @examples
#'
#' # Instantiate from directory
#' corpus <- Corpus$new(name = 'corpus', x = "./data")
#' corpus$summary()
#'
#' # Instantiate from list of Document objects
#' blogs <- Document$new(name = 'blogs', x = "./data/en_US.blogs.txt")
#' news <- Document$new(name = 'news', x = "./data/en_US.news.txt")
#' twitter <- Document$new(name = 'twitter', x = "./data/en_US.twitter.txt")
#'
#' corpus <- Corpus$new(name = 'corpus', x = list(blogs, news, twitter))
#'
#' # Instantiate from Vector: Note this will create subordinate Document
#' # objects for each element of the Vector.  To create a single document
#' # for each vector, create the Document objects first, then attach
#' # to the corpus as above
#' news <- readLines("./data/en_US.news.txt")
#' corpus <- Corpus$new(name  = 'news', x = news)
#'
#' # Attach functionality
#' newCorpus <- Corpus$new(name = 'newCorpus') # Note, can be instantiated with just the name
#' newCorpus$attach(blogs, news, twitter) # Must be Document objects.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, x = NULL) {

      # Initiate logging variables and system meta data
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain and validate parameters
      private$..params$name <- name
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      # Complete Initialization
      private$init(name)

      # Create documents if x is not NULL
      if (!is.null(x)) {
        private$initDocuments(x)
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
