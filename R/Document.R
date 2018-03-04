#' Document
#'
#' \code{Document} Class for creating, managing, reading and writing Documents.
#'
#' Document objects are abstractions of information resources comprised of
#' one or several versions of a single resource or text, each encapsulated in
#' a Text object, as well as Data and information or Analysis objects. This broad
#' definition allows one to "attach" any number of raw, preprocessed,
#' training, or cross-validation Text objects to a single Document. Data
#' objects are the transformations of the original or preprocessed Text
#' object into forms which can be analyzed and studied. A Document's Analysis
#' objects include analyses such as readibility, collocation, document similarity,
#' semantic analysis and so on. This class provides allows users to
#' encapsulate raw text, its conversions into data, and all associated analyses
#' into a single document unit for processing, experimentation, modeling,
#' testing, inference and prediction.
#'
#' @usage myDocument <- Document$new(name = "machineLearning", content = mlAdvances)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Document class.}
#'  }
#' @template entityMethods
#'
#' @param object Text, Data, or Analysis object.
#' @param x A Text, Data or Analyis object, or the name thereto. Used in the detach method.
#' @param cls Class of the object to be detached. Required for detaching objects when the
#' x parameter contains the name of the object.
#' @template ioParams
#' @template metaParams
#'
#' @return Document object, containing related Text, Data, and Analysis objects.
#'
#' @examples
#' rawText <- c("Capsule networks (CapsNets) are a hot new neural net
#'               architecture that may well have a profound impact on deep
#'               learning, in particular for computer vision. Wait, isn't
#'               computer vision pretty much solved already? Haven't we all
#'               seen fabulous examples of convolutional neural networks (CNNs)
#'               reaching super-human level in various computer vision tasks,
#'               such as classification, localization, object detection,
#'               semantic segmentation or instance segmentation.")
#'
#'  raw <- Text$new(name = "capsNets", content = rawText)
#'
#'  mlDoc <- Document$new(name = "capsNets")
#'
#'  m1Doc <- m1Doc$attach(object = raw)
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..attachments = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name) {

      # Initiate logging variables and system meta data
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$created()

      # Obtain and validate parameters
      private$..params$name <- name
      if (private$validateParams()$code == FALSE) stop()

      # Complete Instantiation
      private$..meta$object$name <- name
      private$..id <- private$createId()

      # Create log entry
      private$..state <- paste0("Document object, ", name, ", instantiated.")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                   Aggregation / Assocation Classes                      #
    #-------------------------------------------------------------------------#
    attach = function(object) {

      private$..methodName <- "attach"

      # Obtain and validate parameters
      private$..params$object <- object
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      # Attach
      a <- list()
      a$name <- object$getName()
      a$class <- class(object)[1]
      a[[a$name]] <- object
      private$..attachments[[a$name]] <- a

      # Log
      private$..state <- paste0("Attached ", a$name, " object to ", self$getName(), ".")
      self$logIt()

      invisible(self)

    },

    detach = function(x, cls = NULL) {

      private$..methodName <- "detach"

      private$..params$x <- x
      private$..params$cls <- cls
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      if (class(x)[1] == "character") {
        name <- x
      } else {
        name <- x$getName()
        cls <- class(x)
      }

      list.condition <- sapply(private$..attachments, function(x) (x$name == name & x$class == cls))

      if (exists(private$..attachments[list.condition])) {
        object <- private$..attachments[list.condition]
        private$..attachments[list.condition] <- NULL
        private$..state <- paste0("Dettached ", name, " from ",
                                  self$getName, ".")
        self$logIt()
        return(object)
      } else {
        private$..state <- paste0("Object ", name, " is not attached to ",
                                    self$getName(), ". Returning NULL")
        self$logIt("Warn")
        return(NULL)
      }
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
