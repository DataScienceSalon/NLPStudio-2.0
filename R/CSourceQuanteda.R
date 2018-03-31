#' CSourceQuanteda
#'
#' \code{CSourceQuanteda} Sources a Corpus object from a Quanteda corpus object
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(x, name = NULL)}}{Initializes an object of the CSourceQuanteda class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @examples
#' text <- readtext::readtext(file = d1)
#' q <- quanteda::corpus(text)
#'
#' corpus <- CSource$new(x = q, name = "Joy")$quanteda()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceQuanteda <- R6::R6Class(
  classname = "CSourceQuanteda",
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
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL) {

      private$..params <- list()
      private$..params$x <- x
      if (private$validate("source")$code == FALSE) stop()

      # Name corpus
      if (!is.null(name)) private$..corpus$metadata(key = 'name', value = name)

      # Extract data and metadata
      texts <- x$documents[["texts"]]
      metaData <- x$documents[-which(names(x$documents) == "texts")]
      rownames(metaData) <- NULL

      # Extract a document name variables
      if (is.null(metaData$doc_id)) {
        n <- paste0("document-", seq(1:length(texts)))
      } else {
        n <- metaData$doc_id
        metaData <- metaData %>% select(-doc_id)
      }

      # Create Documents and add to Corpus
      for (i in 1:length(texts)) {

        doc <- TextDocument$new(x = texts[i], name = n[i])
        keys <- names(metaData[i,])
        values <- metaData[i,]
        if (length(keys) > 0) {
          for (j in 1:length(keys)) {
            doc <- doc$meta(key = keys[j], value = values[j])
          }
        }
        private$..corpus <- private$..corpus$addDocument(doc)
      }

      event <- paste0("Corpus sourcing from Quanteda corpus object, complete.")
      private$..corpus$log(cls = class(self)[1], event = event)

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceQuanteda(self)
    }
  )
)
