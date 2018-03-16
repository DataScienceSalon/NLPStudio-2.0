#==============================================================================#
#                               CloneDocument                                  #
#==============================================================================#
#' CloneDocument
#'
#' \code{CloneDocument} Concrete factory for cloning Document objects.
#'
#' Class creates quasi clones of Document objects including certain metadata
#' and text.
#'
#' @section CloneDocument methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant Document object.}
#' }
#'
#' @param x Object to be cloned. Currently supports Corpus and Document objects.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return Document object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Clone Classes
#' @export
CloneDocument <- R6::R6Class(
  "CloneDocument",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Clone,

  public = list(

    #-------------------------------------------------------------------------#
    #                             Instantiation                               #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$..className <- 'CloneDocument'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      # Initiate private members
      private$..in <- x
      private$..name <- name
      private$..out <- Document$new(name = name)

      invisible(self)
    },

    execute = function() {

      private$..methodName <- 'execute'

      # Process metadata
      private$..out <- private$cloneMeta()

      # Process content
      private$..out$content <- private$..in$content

      event <- paste0("Created", class(private$..out)[1], " object '",
                      private$..out$getName(), "' from '",
                      private$..in$getName(), "'.")
      private$..out$log(event = event)

      return(private$..out)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cloneDocument(self)
    }
  )
)
