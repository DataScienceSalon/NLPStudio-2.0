#' CSourceVector
#'
#' \code{CSourceVector} Sources a Corpus object from a character vector or a list of character vectors.
#'
#' Sources a Corpus object from a character vector or a list thereof. Each vector will create
#' a single Text object and one Document object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the CSourceVector class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceVector <- R6::R6Class(
  classname = "CSourceVector",
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
      private$..corpus <- Corpus$new()

      invisible(self)
    },

    #TODO; Complete execute method.  Be mindful of functions that can go into
    # the abstract class.
    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    execute = function() {

      private$..methodName <- 'execute'

      if ("list" %in% class(private$..x)[1]) {
        lapply(private$..x, function(x) {

        })
      }

      # Create Text Object(s)

      # Create Document object(s) and attach Text Object

      # Create Corpus object and attach Document Object

    }
  )
)
