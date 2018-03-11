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
#' @template entityParams
#' @param x A Text, Data or Analyis object to be attached to the Document object.
#' @template metaParams
#'
#' @return Document object.
#'
#' @examples
#'
#' # Instantiate
#' blogsDoc <- Document$new(name = "Blogs")
#'
#' # Attach Text object
#' ## Create Text object
#' blogsTxt <- Text$new(name = 'blogs (raw)')$read(path = "./data/en_US.blogs.txt")
#'
#' ## Attach to Document object
#' blogsDoc$attach(blogsTxt)
#'
#' ## Detach
#' blogsDoc$detach(key ='name', value = 'blogs (raw)')
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite,

  private = list(
    ..attachments = list(),
    ..associates = c("Text", "Data", "Analysis"),

    initText = function(x) {

      if (class(x)[1] == "Text") {
        self$attach(x)
      } else if (length(x) == 1) {
        text <- Text$new(name = tools::file_path_sans_ext(basename(x)), x)
        self$attach(text)
      } else {
        text <- Text$new(name = self$getName(), x)
        self$attach(text)
      }
      return(TRUE)
    },

    summaryShort = function() {

      aSummary <- rbindlist(lapply(private$summarizeAttachments(quiet = TRUE), function(a) {
        attachments <- list()
        attachments$class <- unique(a$class)
        attachments$N <- nrow(a)
        attachments
      }))
      name <- aSummary$class
      aSummary <- aSummary %>% select(N)
      names(aSummary) <- name

      short <- data.frame(class = private$..meta$object$class,
                          id = private$..meta$object$id,
                          name = private$..meta$object$name,
                          desc = private$..meta$object$desc,
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      short <- cbind(short, aSummary)
      other <- data.frame(created = private$..meta$system$created,
                          user = private$..meta$system$user,
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      short <- cbind(short, other)
      return(short)
    }

  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      # Initiate logging variables and system meta data
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Complete Document Initialization
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
