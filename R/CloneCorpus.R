#==============================================================================#
#                               CloneCorpus                                    #
#==============================================================================#
#' CloneCorpus
#'
#' \code{CloneCorpus} Concrete factory for cloning Corpus objects.
#'
#' Class creates quasi clones of Corpus objects. Attachments are not cloned
#' within this Class.  The Documents attached to a corpus must be cloned
#' then attached.
#'
#' @section CloneCorpus methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant Corpus object.}
#' }
#'
#' @param x Object to be cloned. Currently supports Corpus and Document objects.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return Corpus object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Clone Classes
#' @export
CloneCorpus <- R6::R6Class(
  "CloneCorpus",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Clone,

  public = list(

    #-------------------------------------------------------------------------#
    #                             Instantiation                               #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$..className <- 'CloneCorpus'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()

      # Initiate private members
      private$..in <- x
      private$..name <- name
      private$..out <- Corpus$new(name = name)

      invisible(self)
    },

    execute = function() {

      private$..methodName <- 'execute'

      # Process metadata
      private$..out <- private$cloneMeta()

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
      visitor$cloneCorpus(self)
    }
  )
)
