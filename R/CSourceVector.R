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

  private = list(
    ..x = character(),
    ..name = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      # Initiate logging variables and system meta data
      private$..className <- 'CSourceVector'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()
      private$..params <- list()

      # Save parameter and create Corpus object.
      private$..x <- x
      private$..name <- name
      private$..corpus <- Corpus$new(name = name)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    execute = function() {

      private$..methodName <- 'execute'

      if ("list" %in% class(private$..x)[1]) {
        lapply(private$..x, function(x) {
          name <- names(x)
          txt <- Text$new(name = name, x = x)
          txt$state <- 'raw'
          doc <- Document$new(name = name)
          doc <- doc$attach(txt)
          private$..corpus$attach(x = doc)
        })
      } else {
        name <- names(private$..x)
        txt <- Text$new(name = name, x = private$..x)
        txt$state <- 'raw'
        doc <- Document$new(name = name)
        doc <- doc$attach(txt)
        private$..corpus$attach(x = doc)
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
