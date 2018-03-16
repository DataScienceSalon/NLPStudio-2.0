#' CSourceTM
#'
#' \code{CSourceTM} Sources a Corpus object from a TM corpus object
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(x, name = NULL)}}{Initializes an object of the CSourceTM class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#'  tmc <- tm::VCorpus(DirSource("./foo))
#'  corpus <- CSource$new(x = tmc, name = "Alexandra")$tm()
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceTM <- R6::R6Class(
  classname = "CSourceTM",
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
      private$..className <- 'CSourceTM'
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

      docNames <- names(private$..x)

      lapply(seq_along(private$..x), function(x) {

        # Create Text and Document Objects
        content <- private$..x[[x]]$content
        doc <- Document$new(x =content, name = docNames[x])

        # Create metadata
        for (i in 1:length(private$..x[[x]]$meta)) {
          if (length(private$..x[[x]]$meta[[i]]) > 0) {
            if (names(private$..x[[x]]$meta[i]) == 'datetimestamp') {
              doc$meta(key = 'tmCreated', value = private$..x[[x]]$meta[[i]])
            } else {
              doc$meta(key = names(private$..x[[x]]$meta[i]),
                        value = private$..x[[x]]$meta[[i]])
            }
          }
        }

        # Attach Document
        private$..corpus$attach(doc)
      })
      event <- paste0("Corpus sourcing from tm Corpus object, complete.")
      private$..corpus$log(event = event)

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceTM(self)
    }
  )
)
