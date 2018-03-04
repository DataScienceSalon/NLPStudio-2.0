#' Text
#'
#' \code{Text} Class for creating, managing, reading and writing text content and metadata.
#'
#' Class contains text content and metadata for the Text object.
#' The object is instantiated with a name and a character
#' vector containing the text. Once the object is created, users may
#' add, change and remove metadata via key / value pairs. IO
#' methods support reading and writing the texts in a variety
#' of formats using the \code{\link[NLPStudio]{IOStrategy}} factory
#' class.
#'
#' @usage myText <- Text$new(name = "skiReport", content = avalanche)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Text class.}
#'  }
#' @template entityMethods
#'
#' @param content Character vectors containing actual content of the Text object.
#' @template ioParams
#' @template metaParams
#'
#' @return Text object, containing the text content, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' avalanche <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' myText <- Text$new(name = 'skiReport', content = avalanche)
#'
#' myText <- myText$meta(key = c("author", "editor", "year"),
#'                       value = c("Dorfmeister", "Huffington", "2018"))
#'
#' myText$meta()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Text <- R6::R6Class(
  classname = "Text",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$accessed()
        return(private$..content)

      } else {
        if (!("character" %in% class(value))) {
          private$..state <- "Text must be of the 'character' class."
          self$logIt("Error")
          stop()
        } else {

          private$..content <- value
          private$modified()
          private$..state <- "Updated text content."
          self$logIt()
        }
      }
      invisible(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, content) {

      # Initiate logging variables and system meta data
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$created()

      # Obtain and validate parameters
      private$..params$name <- name
      private$..params$content <- content
      if (private$validateParams()$code == FALSE) stop()

      # Complete Instantiation
      private$..meta$object$name <- name
      private$..content <- content
      private$..id <- private$createId()

      # Create log entry
      private$..state <- paste0("Text object, ", name, ", instantiated.")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$text(self)
    }
  )
)
