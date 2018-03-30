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

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {

      # Initiate logging variables and system meta data
      private$loadDependencies()
      private$..corpus <- Corpus$new()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Source Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL) {

      private$..params <- list()
      private$..params$x <- x
      if (private$validate("source")$code == FALSE) stop()

      if (!is.null(name)) private$..corpus$metadata(key = 'name', value = name)

      docNames <- names(x)

      lapply(seq_along(x), function(y) {

        # Create Text and Document Objects
        content <- x[[y]]$content
        doc <- Document$new(x =content, name = docNames[y])

        # Create metadata
        for (i in 1:length(x[[y]]$meta)) {
          if (length(x[[y]]$meta[[i]]) > 0) {
            if (names(x[[y]]$meta[i]) == 'datetimestamp') {
              doc$metadata(key = 'tmCreated', value = x[[y]]$meta[[i]])
            } else {
              doc$metadata(key = names(x[[y]]$meta[i]),
                        value = x[[y]]$meta[[i]])
            }
          }
        }

        # Attach Document
        private$..corpus$addDocument(doc)
      })
      event <- paste0("Corpus sourcing from tm Corpus object, complete.")
      private$..corpus$log(cls = class(self)[1], event = event)

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
