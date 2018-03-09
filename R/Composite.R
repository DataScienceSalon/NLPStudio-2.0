#==============================================================================#
#                               Composite                                      #
#==============================================================================#
#' Composite
#'
#' \code{Composite} Base class for all Composite classes
#'
#' This base class defines members and methods common across all the Composite
#' classes NLPStudio, Pipeline, Corpus and Document.
#'
#' @section Composite methods:
#'  \itemize{
#'   \item{\code{attach(x)}}{Method for attaching objects.}
#'   \item{\code{detach(key, value)}}{Method for detaching objects.}
#'   \item{\code{attachments(key = NULL, value = NULL)}}{Lists an object's attachments.}
#'  }
#'
#'  @param x Object to be attached
#'  @param key Character string or vector of strings indicating the metadata
#'  variable or variables used for matching
#'  @param value Character string or vector of strings indicating the
#'  metadata value associated with the key(s) parameter.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Composite <- R6::R6Class(
  classname = "Composite",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..attachments = list(),
    #-------------------------------------------------------------------------#
    #                             Search Method                               #
    #-------------------------------------------------------------------------#
    # search
    #
    # This method enables users to query attachments using metadata. The method
    # performs a search based on the key/value pair parameters and returns
    # a boolean vector, indexing into the list of attachments, the element
    # or elements that match the search criteria.
    #
    search = function() {

      # get Params
      key <- private$..params$key
      value <- private$..params$value

      if (length(key) == 1) {
        listCondition <- sapply(private$..attachments, function(a) {
          unlist(unname((a$meta()$object[key] == value)))
          })
      } else {
        listCondition <- sapply(private$..attachments, function(a) {
          sapply(seq_along(key), function(k) {
            unlist(unname((a$meta()$object[key[k]] == value[k])))
        })
      })
     }

      return(listCondition)
    },

    summaryAttachments = function() {

    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                                 Attach                                  #
    #-------------------------------------------------------------------------#
    attach = function(x) {

      private$..methodName <- "attach"

      # Obtain and validate parameters
      private$..params <- list()
      private$..params$x <- x
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      # Obtain and flatten meta data into list, append the object, then add to attachments.
      id <- x$getId()
      private$..attachments[[id]] <- x

      # Update date/time metadata and create log entry
      private$modified()
      private$..state <- paste0("Attached ", a$name, " object to ", self$getName(), ".")
      self$logIt()

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                                 Detach                                  #
    #-------------------------------------------------------------------------#
    detach = function(key, value) {

      private$..methodName <- "detach"

      private$..params <- list()
      private$..params$key <- key
      private$..params$value <- value
      if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

      listCondition <- private$search()

      if (exists(private$..attachments[listCondition])) {
        object <- private$..attachments[listCondition]
        private$..attachments[listCondition] <- NULL
        private$modified()
        private$..state <- paste0("Detached ", object$getName, " from ",
                                  self$getName, ".")
        self$logIt()
      } else {
        object <- NULL
        self$access()
        private$..state <- paste0("Object is not attached to ",
                                  self$getName(), ". Returning NULL")
        self$logIt("Warn")
        return(object)
      }
    },

    #-------------------------------------------------------------------------#
    #                         Get Attachment Method                           #
    #-------------------------------------------------------------------------#
    get = function(key = NULL, value = NULL) {

      private$..methodName <- "get"

      # Obtain and validate parameters
      private$..params <- list()
      private$..params$key <- key
      private$..params$value <- value
      if (private$validateParams(private$..methodName)$code == FALSE) stop()

      # If parameters are null, return all attachments, otherwise search.
      if (is.null(key)) {
        objects <- private$..attachments
      } else {
        listCondition <- private$search()
        objects <- private$..attachments[listCondition]
      }

      private$accessed()

      return(objects)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(meta = TRUE, stats = TRUE, system = TRUE, quiet = FALSE,
                       abbreviated = FALSE, attachments = TRUE) {
      if (abbreviated) {
        result <- private$summaryShort()
      } else {
        result <- list()
        section <- character()

        if (meta) {
          result$meta <- private$summaryObjMeta(quiet = quiet)
          section <- c(section, "Metadata")
        }

        if (attachments) {
          result$attachments <- private$summaryAttachments(quiet = quiet)
          section <- c(section, "Attachments")
        }

        if (system) {
          result$sys <- private$summarySysInfo(quiet = quiet)
          section <- c(section, "System Info")
        }

        names(result) <- section
      }
      return(result)
    }
  )
)
