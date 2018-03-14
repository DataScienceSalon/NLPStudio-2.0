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
      private$..className <- 'CSourceQuanteda'
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

      # Extract data and metadata
      texts <- private$..x$documents[["texts"]]

      metaData <- private$..x$documents[-which(names(private$..x$documents) == "texts")]
      rownames(metaData) <- NULL

      # Extract a name variable
      if (is.null(metaData$doc_id)) {
        n <- paste0(private$..name, "-document-", seq(1:length(texts)))
      } else {
        n <- metaData$doc_id
        metaData <- metaData %>% select(-doc_id)
      }

      # Create Documents and add to Corpus
      for (i in 1:length(texts)) {

        doc <- Document$new(x = texts[i], name = n[i])
        doc$state <- 'raw'
        keys <- names(metaData[i,])
        values <- metaData[i,]
        if (length(keys) > 0) {
          for (j in 1:length(keys)) {
            doc <- doc$meta(key = keys[j], value = values[j])
          }
        }
        private$..corpus <- private$..corpus$attach(doc)
      }


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
