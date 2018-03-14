#' CSourceDir
#'
#' \code{CSourceDir} Sources a Corpus object from a directory source.
#'
#' Sources a Corpus object from a directory source. Each file yields a single Document
#' object and a single associated Text object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CSourceDir class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @examples
#' dir <- "./foo
#' corpus <- CSource$new(x = dir, name = "Foo")$dir()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceDir <- R6::R6Class(
  classname = "CSourceDir",
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
      private$..className <- 'CSourceDir'
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

      if (isDirectory(private$..x)) {
        files <- list.files(private$..x, full.names = TRUE)
      } else {
        glob <- basename(private$..x)
        dir <- dirname(private$..x)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      lapply(files, function(f) {
        name <- basename(f)
        content <- IO$new()$read(f, repair = TRUE)
        txt <- Text$new(name = name, x = content)
        txt$state <- 'raw'
        doc <- Document$new(name = name)
        doc <- doc$attach(txt)
        private$..corpus$attach(x = doc)
      })

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceDir(self)
    }
  )
)
