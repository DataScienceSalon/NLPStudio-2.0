#' CSource
#'
#' \code{CSource} Class for sourcing Corpus objects.
#'
#' CSource is the client facing class used to source a Corpus object from a variety
#' of formats. Methods support sourcing Corpora objects from character vectors,
#' JSON, and XML files or directories. Corpora may be imported from other
#' packages such as Quanteda, TM, KoRpus, and qdap.
#'
#' @usage myCorpus <- CSource$new(x)$vector()
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the CSource class.}
#'   \item{\code{vector()}}{Sources the Corpus object from a character vector.}
#'   \item{\code{file()}}{Sources the Corpus object from a file.}
#'   \item{\code{directory()}}{Sources the Corpus object from a directory.}
#'   \item{\code{json()}}{Sources the Corpus object from a json source.}
#'   \item{\code{xml()}}{Sources the Corpus object from an xml source.}
#'   \item{\code{quanteda()}}{Sources the Corpus object from a Quanteda corpus object.}
#'   \item{\code{tm()}}{Sources the Corpus object from a tm VCorpus object. }
#'   \item{\code{korpus()}}{Sources the Corpus object from a koRpus corpus object.}
#'   \item{\code{qdap()}}{Sources the Corpus object from a qdap corpus object.}
#'  }
#'
#' @param x Character vector containing text, the name of a file, or directory
#' or a corpus object from a supported package.
#'
#' @return Corpus object.
#'
#' @examples
#' corpus <- CSource$new(x)$vector()
#' corpus <- CSource$new(x)$file()
#' corpus <- CSource$new(x)$directory()
#' corpus <- CSource$new(x)$json()
#' corpus <- CSource$new(x)$xml()
#' corpus <- CSource$new(x)$quanteda()
#' corpus <- CSource$new(x)$tm()
#' corpus <- CSource$new(x)$koRpus()
#' corpus <- CSource$new(x)$qdap()
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSource <- R6::R6Class(
  classname = "CSource",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Base,

  private = list(
    ..x = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      # Initiate logging variables and system meta data
      private$..className <- 'CSource'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Complete Initialization
      private$..x <- x

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Source Methods                               #
    #-------------------------------------------------------------------------#
    vector = function() {
      private$..methodName = 'vector'
      corpus <- CSourceVector$new(x = private$..x)$execute()
      return(corpus)
    },

    file = function() {
      private$..methodName = 'file'
      corpus <- CSourceFile$new(private$..x)$execute()
      return(corpus)
    },

    dir = function() {
      private$..methodName = 'dir'
      corpus <- CSourceDir$new(private$..x)$execute()
      return(corpus)
    },

    json = function() {
      private$..methodName = 'json'
      stop("This method is currently unsupported")
    },

    xml = function() {
      private$..methodName = 'xml'
      stop("This method is currently unsupported")
    },

    quanteda = function() {
      private$..methodName = 'quanteda'
      corpus <- CSourceQuanteda$new(private$..x)$execute()
      return(corpus)
    },

    tm = function() {
      private$..methodName = 'tm'
      corpus <- CSourceTM$new(private$..x)$execute()
      return(corpus)
    },

    korpus = function() {
      private$..methodName = 'korpus'
      corpus <- CSourceKorpus$new(private$..x)$execute()
      return(corpus)
    },

    qdap = function() {
      private$..methodName = 'qdap'
      corpus <- CSourceQdap$new(private$..x)$execute()
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csource(self)
    }
  )
)
