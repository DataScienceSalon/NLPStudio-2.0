#==============================================================================#
#                                   Clone                                      #
#==============================================================================#
#' Clone
#'
#' \code{Clone} Class responsible for cloning primitive and composite objects.
#'
#' Class clones primitive and composite objects. Currently supports
#' Document and Corpus objects.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant object.}
#' }
#'
#' @param x Object to be cloned. Currently supports Corpus and Document objects.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return A Replicant of the input object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @export
Clone <- R6::R6Class(
  "Clone",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Base,

  private = list(

    validate = function(x) {
      private$..params <- list()
      private$..params$x <- x
      return(private$validate())
    },

    cloneMeta = function(x, out) {
      meta <- x$meta()
      keys <- names(meta$object)
      values <- as.character(meta$object)
      values <- values[!keys %in% c("id", "name", "class")]
      keys <- keys[!keys %in% c("id", "name", "class")]
      out$meta(key = keys, value = values)
      return(out)
    },

    cloneDocument = function(x, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Document$new(name = name)
      out <- private$cloneMeta(x, out)

      out$content <- x$content
      event <- paste0("Created", class(out)[1], " object '",
                      out$getName(), "' from '",
                      x$getName(), "'.")
      out$log(cls = class(self)[1], event = event)
      return(out)
    },

    cloneCorpus = function(x, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Corpus$new(name = name)
      out <- private$cloneMeta(x, out)

      # Process attachments
      attachments <- x$getDocument()
      lapply(attachments, function(a) {
        attachment <- private$cloneDocument(x = a)
        out <<- out$attach(attachment)
      })

      event <- paste0("Created", class(out)[1], " object '",
                      out$getName(), "' from '",
                      x$getName(), "'.")
      out$log(cls = class(self)[1], event = event)
      return(out)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function() { invisible(self) },
    document = function(x, name = NULL) {

      private$..className <- 'Clone'
      private$..methodName <- 'document'
      private$logR <- LogR$new()

      # Validate parameters
      if (private$validate(x)$code == FALSE) stop()

      out <- private$cloneDocument(x = x, name = name)
      return(out)
    },

    corpus = function(x, name = NULL) {

      private$..className <- 'Clone'
      private$..methodName <- 'corpus'
      private$logR <- LogR$new()

      # Validate parameters
      if (private$validate(x)$code == FALSE) stop()

      out <- private$cloneCorpus(x = x, name = name)
      return(out)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$klone(self)
    }
  )
)
