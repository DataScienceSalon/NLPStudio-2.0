#==============================================================================#
#                               Collection                                     #
#==============================================================================#
#' Collection
#'
#' \code{Collection} Base class for all Collection classes
#'
#' This base class defines members and methods common across all the Collection
#' classes, such as Corpus, Data, Analysis, Model and Evaluation classes.
#'
#' @section Collection methods:
#'  \itemize{
#'   \item{\code{attach(x)}}{Method for attaching objects.}
#'   \item{\code{detach(key, value)}}{Method for detaching objects.}
#'   \item{\code{attachments(key = NULL, value = NULL)}}{Lists an object's attachments.}
#'  }
#'
#'  @param x Object or list of objects to be attached
#'  @param key Character string or vector of strings indicating the metadata
#'  variable or variables used for matching
#'  @param value Character string or vector of strings indicating the
#'  metadata value associated with the key(s) parameter.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Collection Classes
#' @export
Collection <- R6::R6Class(
  classname = "Collection",
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
      k <- private$..params$key
      v <- private$..params$value
      cls <- private$..params$class
      listCondition <- NULL

      if (!is.null(private$..attachments[[cls]])) {
        if (length(k) == length(v)) {
          listCondition <- sapply(private$..attachments[[cls]], function(a) {
            sapply(seq_along(key), function(x) {
              (a$meta()$object[[k[x]]] == v[x])
            })
          })
        } else {
          listCondition <- sapply(private$..attachments[[cls]], function(a) {
            (a$meta()$object[[k]] %in%  v)
          })
        }
      }

      return(listCondition)
    },

    #-------------------------------------------------------------------------#
    #                            Summary Methods                              #
    #-------------------------------------------------------------------------#
    summarizeAttachments = function(quiet = FALSE) {
      attachments <- list()
      sections <- names(private$..attachments)

      if (!is.null(sections)) {
        for (i in 1:length(sections)) {
          attachments[[sections[i]]] <- rbindlist(lapply(private$..attachments[[sections[[i]]]], function(a) {
            as.data.frame(a$summary(abbreviated = TRUE))
          }))

          if (quiet == FALSE) {
            cat("\n\n", paste0(sections[i]), "\n")
            print(attachments[[sections[i]]], row.names = FALSE)
          }
        }
        names(attachments) <- sections
      }

      return(attachments)
    },

    #-------------------------------------------------------------------------#
    #                                 Attach                                  #
    #-------------------------------------------------------------------------#
    attach = function(x) {

      private$..methodName <- "attach"

      doAttach = function(a) {
        # Obtain and validate parameters
        private$..params <- list()
        private$..params$x <- a
        if (private$validateParams(what = private$..methodName)$code == FALSE) stop()

        # Get document id and class, then attach.
        id <- a$getId()
        attachment <- list(a)
        names(attachment) <- id
        cls <- class(x)[1]

        if (!is.null(private$..attachments[[cls]])) {
          private$..attachments[[cls]] <- attachment
        } else {

          private$..attachments[[cls]] <- list()
          private$..attachments[[cls]] <- attachment
        }

        # Update date/time metadata and create log entry
        private$modified()
        private$..event <- paste0("Attached ", a$getName(), " object to ", self$getName(), ".")
        private$logIt()
      }

      if ("list" %in% class(x)[1]) {
        lapply(x, function(a) {
          doAttach(a)
        })
      } else {
        doAttach(x)
      }

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                                 Detach                                  #
    #-------------------------------------------------------------------------#
    detach = function(x) {

      private$..methodName <- "detach"

      id <- x$getId()
      cls <- class(x)[1]
      if (!is.null(private$..attachments[[cls]])) {
        if (!is.null(private$..attachments[[cls]][[id]])) {
          private$..attachments[[cls]][[id]] <- NULL
          private$modified()
          private$..event <- paste0("Detached ", x$getName, " from ",
                                    self$getName, ".")
          private$logIt()
        } else {
          self$access()
          private$..event <- paste0("Object is not attached to ",
                                    self$getName(), ". Returning NULL")
          private$logIt("Warn")
        }
      } else {
        self$access()
        private$..event <- paste0("Object is not attached to ",
                                  self$getName(), ". Returning NULL")
        private$logIt("Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Get Attachment Method                           #
    #-------------------------------------------------------------------------#
    get = function(cls, key = NULL, value = NULL) {

      private$..methodName <- "get"

      # Obtain and validate parameters
      private$..params <- list()
      private$..params$key <- key
      private$..params$value <- value
      private$..params$class <- cls
      if (private$validateParams(private$..methodName)$code == FALSE) stop()

      # If parameters are null, return all attachments, otherwise search.
      objects <- NULL
      if (is.null(key)) {
        if (!is.null(private$..attachments[[cls]])) {
          objects <- private$..attachments[[cls]]
        }
      } else {
        listCondition <- private$search()
        if (!is.null(listCondition)) {
          objects <- private$..attachments[[cls]][listCondition]
        }
      }

      private$accessed()

      return(objects)
    }

  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(meta = TRUE, stats = TRUE, system = TRUE, state = TRUE,
                       quiet = FALSE, abbreviated = FALSE, attachments = TRUE) {
      if (abbreviated) {
        result <- private$summaryShort()
      } else {
        result <- list()
        section <- character()

        if (meta) {
          result$meta <- private$summaryObjMeta(quiet = quiet)
          section <- c(section, "Metadata")
        }

        if (state) {
          result$app <- private$summaryState(quiet = quiet)
          section <- c(section, "State Info")
        }

        if (attachments) {
          result$attachments <- private$summarizeAttachments(quiet = quiet)
          section <- c(section, "Attachments")
        }

        if (system) {
          result$sys <- private$summarySysInfo(quiet = quiet)
          section <- c(section, "System Info")
        }

        names(result) <- section
      }
      invisible(result)
    }
  )
)
