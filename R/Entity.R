#==============================================================================#
#                               Entity                                         #
#==============================================================================#
#' Entity
#'
#' \code{Entity} Abstract including methods common to all Entity classes.
#'
#' Class provides methods common to entity classes, such as Corpus Document, DFM,
#' POS, and NGram.
#'
#' @section Entity methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract class.}
#'   \item{\code{summary()}}{Summarizes an object in terms of its meta data.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Entity <- R6::R6Class(
  classname = "Entity",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..params = list(),

    logR =  character(),
    validator = character(),
    meta = character(),

    loadDependencies = function(name = NULL) {
      private$logR <- LogR$new()
      private$validator <- Validator$new()
      private$meta <- Meta$new()
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                      Initialize Metadata Method                         #
    #-------------------------------------------------------------------------#
    initMeta = function(name = NULL) {

      # Creates unique identifier and create object metadata
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      hashid <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                                  sample(1:1000, 1, replace = TRUE), settings)
      name <-  ifelse(is.null(name), paste0(class(self)[1]," (", toupper(hashid),
                                           ")"), name)
      id <-  toupper(hashid)
      cls <- class(self)[1]
      description <- paste0(cls, " object '", name, "' created on ",
                            format(Sys.Date(), "%a %b %d %Y"),
                            " by ", Sys.info()[['user']], ".")


      private$meta$created(id = id, name = name, cls = cls, description = description)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Validation Method                              #
    #-------------------------------------------------------------------------#
    validate = function(what = "initialize") {

      if (what == "initialize") {
        status <- private$validator$init(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      } else if (what == "attach") {
        status <- private$validator$attach(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      }  else if (what == "detach") {
        status <- private$validator$detach(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      } else if (what == "read") {
        status <- private$validator$read(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      } else if (what == "source") {
        status <- private$validator$source(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      } else if (what == "metadata") {
        status <- private$validator$metadata(self)
        if (status$code == FALSE) {
          private$logR$log(cls = class(self)[1], event = status$msg, level = "Error")
        }
      }
      return(status)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    oneLiner = function(meta, quiet = FALSE) {

      df <- data.frame(class = meta$core$class,
                       id = meta$core$id,
                       name = meta$core$name,
                       description = meta$core$description,
                       created = meta$state$created,
                       user = meta$system$user,
                       stringsAsFactors = FALSE,
                       row.names = NULL)
      return(df)
    },

    core = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$core, stringsAsFactors = FALSE,
                          row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\n\nObject Id    : ", meta$core$id))
        cat(paste0("\nObject Class : ", meta$core$class))
        cat(paste0("\nObject Name  : ", meta$core$name))
        cat(paste0("\nDescription  : ", meta$core$description))

        otherMeta <- df %>% select(-id, -class, -name, -description)
        if (ncol(otherMeta) > 0) {
          cat("\n\nAdditional Core Metadata:\n")
          print(otherMeta, row.names = FALSE)
        }
      }
      return(df)
    },

    state = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$state, stringsAsFactors = FALSE,
                          row.names = NULL)
      if (quiet == FALSE) {
        if (ncol(df) > 0) {
          cat("\n\nState Information: \n")
          print(df, row.names = FALSE)
        }
      }
      return(df)
    },

    system = function(meta, quiet = FALSE) {
      df <- as.data.frame(meta$system, stringsAsFactors = FALSE,
                          row.names = NULL)
      if (quiet == FALSE) {
        if (ncol(df) > 0) {
          cat("\nSystem Information: \n")
          print(df, row.names = FALSE)
        }
      }
      return(df)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this base class.")},

    #-------------------------------------------------------------------------#
    #                         Basic Get Methods                               #
    #-------------------------------------------------------------------------#
    getId = function() { unlist(private$meta$get(key = "id")) },
    getName = function() { unlist(private$meta$get(key = "name")) },
    getParams = function() { private$..params },

    #-------------------------------------------------------------------------#
    #                           Query Method                                  #
    #-------------------------------------------------------------------------#
    query = function(cls, key, value) {

      if (!(cls %in% class(self))) {
        return(FALSE)
      }

      if (is.null(key) | is.null(value)) {
        return(FALSE)
      }

      md <- private$meta$get()
      for (i in 1:length(md)) {
        mdl <- as.list(md[[i]])
        n <- names(mdl)
        for (j in 1:length(mdl)) {
          if ((n[j]  == key) & (sum(mdl[[j]]  %in% value) > 0)) {
            return(TRUE)
          }
        }
      }
      return(FALSE)
    },

    #-------------------------------------------------------------------------#
    #                        Generic Summary Method                           #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, state = TRUE, system = TRUE,
                       quiet = FALSE, abbreviated = FALSE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta, quiet = quiet)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta,  quiet = quiet)
          section <- c(section, "Core Metadata")
        }

        if (state) {
          result$state <- private$state(meta, quiet = quiet)
          section <- c(section, "State Information")
        }

        if (system) {
          result$sys <- private$system(meta, quiet = quiet)
          section <- c(section, "System Information")
        }

        names(result) <- section
      }

      private$meta$accessed()

      invisible(result)
    },

    #-------------------------------------------------------------------------#
    #                             Metadata Method                             #
    #-------------------------------------------------------------------------#
    metadata = function(key = NULL, value = NULL) {

      private$..params <- list()
      private$..params$key <- key
      private$..params$value <- value
      v <- private$validate("metadata")
      if (v$code == FALSE) stop()

      if (is.null(key)) {
        print(private$meta$get())
      } else {
        private$meta$set(key = key, value = value)
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Log Method                                  #
    #-------------------------------------------------------------------------#
    log = function(event = NULL, cls = NULL, method = NULL) {
      if (is.null(event)) {
        print(private$..log)
      } else {
        private$logR$log(cls = cls, event = event)
      }
    }
  )
)
