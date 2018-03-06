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
#'  capsNets <- Document$new(name = "capsNets", content = rawText)
#'
#'  mlCorpus <- Corpus$new(name = "machineLearning")
#'
#'  m1Corpus <- mlCorpus$attach(object = capsNets)
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

  private = list(
    ..attachments = list()

  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name) {

      # Initiate logging variables and system meta data
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain and validate parameters
      private$..params$name <- name
      if (private$validateParams()$code == FALSE) stop()

      # Complete Instantiation
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                   Aggregation / Association Classes                     #
    #-------------------------------------------------------------------------#
    attach = function(x) {

      private$..methodName <- "attach"

      # Obtain and validate parameters
      private$..params$x <- x
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      # Attach
      name <- x$getName()
      private$..attachments[[name]] <- x

      # Log
      private$..state <- paste0("Attached ", name, " object to ", self$getName(), ".")
      private$modified()
      self$logIt()

      invisible(self)

    },

    detach = function(x) {

      private$..methodName <- "detach"

      private$..params$x <- x
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      if (class(x)[1] == "character") {
        name <- x
      } else {
        name <- x$getName()
      }

      if (exists(private$..attachments[name])) {
        object <- private$..attachments[[name]]
        private$..attachments[list.condition] <- NULL
        private$..state <- paste0("Dettached ", name, " from ",
                                  self$getName, ".")
        self$logIt()
        private$modified()
      } else {
        object <- NULL
        private$..state <- paste0("Object ", name, " is not attached to ",
                                    self$getName(), ". Returning NULL")
        self$logIt("Warn")
        private$accessed()
      }
      return(object)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
    }
  )
)
