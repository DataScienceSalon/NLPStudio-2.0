#==============================================================================#
#                                   Clone                                      #
#==============================================================================#
#' Clone
#'
#' \code{Clone} Abstract factory class that creates a clone of an object.
#'
#' Given a Corpus or Document object, and an optional name, this class produces
#' a replica of the object via the appropriate concrete factory class, then
#' returns the replicant to the calling environment. The clones are actually
#' quasi clones as the replicants have their own identifiers, names,
#' and descriptions.
#'
#' @section Clone methods:
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
#' @family Clone Classes
#' @export
Clone <- R6::R6Class(
  "Clone",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Base,

  private = list(
    ..in = character(),
    ..name = character(),
    ..out = character(),

    cloneMeta = function() {

      meta <- private$..in$meta()
      keys <- names(meta$object)
      values <- as.character(meta$object)
      values <- values[!keys %in% c("id", "class")]
      keys <- keys[!keys %in% c("id", "class")]
      private$..out$meta(key = keys, value = values)
      return(private$..out)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Instantiation                               #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$..className <- 'Clone'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      # Initiate private members
      private$..in <- x
      private$..name <- name

      invisible(self)
    },

    execute = function() {

      private$..methodName <- 'execute'

      # Invoke appropriate concrete factory object and obtain replicant
      if (class(private$..in)[1] == "Corpus") {
        private$..out <- CloneCorpus$new(x = private$..in,
                                         name = private$..name)$execute()
      } else {
        private$..out <- CloneDocument$new(x = private$..in,
                                         name = private$..name)$execute()
      }

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
      visitor$klone(self)
    }
  )
)
