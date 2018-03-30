#' CSourceVector
#'
#' \code{CSourceVector} Sources a Corpus object from a character vector or a list of character vectors.
#'
#' Sources a Corpus object from a character vector or a list thereof. Each vector will create
#' a single Text object and one Document object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(x, name = NULL)}}{Initializes an object of the CSourceVector class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#' @param concatenate Logical for the vector method. If TRUE, character vectors are
#' concatenated into a single text for the Document object.
#'
#' @examples
#' text <- c("The firm possesses unparalleled leverage in Washington,
#' thanks in part to its track record of funneling executives into
#' senior government posts. Even the Trump administration, which
#' rode a populist wave to electoral victory, is stocked with
#' Goldman alumni, including Treasury Secretary Steven Mnuchin and
#' the departing White House economic adviser Gary D. Cohn.",
#' "Goldman is also an adviser to many of America’s — and the
#' world’s — largest companies, ranging from stalwarts like Walt
#' Disney to upstarts like Uber.")
#'
#' corpus <- CSource$new(x = txt, name = "Goldman")$vector()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceVector <- R6::R6Class(
  classname = "CSourceVector",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CSource0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()

      private$..corpus <- Corpus$new()

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL, concatenate = TRUE) {

      private$..params <- list()
      private$..params$x <- x
      private$..params$name <- name

      if (private$validate("source")$code == FALSE) stop()

      if ("list" %in% class(x)[1]) {
        lapply(x, function(y) {
          name <- names(y)
          doc <- Document$new(x = y, name = name)
          private$..corpus$addDocument(x = doc)
        })
      } else if (concatenate) {
        name <- names(x)
        doc <- Document$new(x = x, name = name)
        private$..corpus$addDocument(x = doc)
      } else {
        for (i in 1:length(x)) {
          name <- names(x[i])
          if (is.null(name)) {
            name <- paste0("Document-",i)
          }
          doc <- Document$new(x = x[i], name = name)
          private$..corpus <- private$..corpus$addDocument(x = doc)
        }
      }

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceVector(self)
    }
  )
)
