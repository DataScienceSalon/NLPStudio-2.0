#==============================================================================#
#                               Entity                                         #
#==============================================================================#
#' Entity
#'
#' \code{Entity} Base class for all entity related classes
#'
#' This base class defines members and methods common across all entity related
#' classes, including Studio, Corpus, Set, and Document classes.
#'
#' @section Entity methods:
#'  \itemize{
#'   \item{\code{getId()}}{Returns the identifier for the object.}
#'   \item{\code{getParams()}}{Returns the parameters passed to the constructor.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Entity <- R6::R6Class(
  classname = "Entity",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Base,

  private = list(
    ..meta = list(
      object = list(),
      stats = list(),
      system = list()
    ),
    #-------------------------------------------------------------------------#
    #                                 Init                                    #
    #-------------------------------------------------------------------------#
    # init
    #
    #  Method that standardizes the instantiation of domain objects
    #
    # This method is called by constructors once the parameters have been
    # validated.  It assigns a unique identifier to the object, adds the
    # identifier, name and the object's class to its metadata, logs the
    # creation and returns control to the constructor.

    init = function(name = NULL) {

      # Clear parameters variables and set datetime stamps
      private$created()

      # Creates unique identifier and update metadata as appropriate
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      hashid <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                                  sample(1:1000, 1, replace = TRUE), settings)
      private$..meta$object$name <- ifelse(is.null(name),
                                           paste0(class(self)[1]," (", toupper(hashid),
                                                  ")"), name)
      private$..meta$object$id <- toupper(hashid)
      private$..meta$object$class <- class(self)[1]
      private$..meta$object$description <- paste0(private$..meta$object$class,
                                                  " object, '", private$..meta$object$name,
                                                  "', created on ",
                                                  format(Sys.Date(), "%a %b %d %Y"),
                                                  " by ", Sys.info()[['user']], ".")

      # Log the entry
      private$..state <- paste0(private$..className, " object, ", name, ", instantiated.")
      self$logIt()
    },


    #-------------------------------------------------------------------------#
    #                   Object Date/Time Management                           #
    #-------------------------------------------------------------------------#
    # Updates system metadata when object has been accessed
    accessed = function() {
      private$..meta$system$user <- Sys.info()[["user"]]
      private$..meta$system$machine <- Sys.info()[["machine"]]
      private$..meta$system$os <- Sys.info()[["sysname"]]
      private$..meta$system$release <- Sys.info()[["release"]]
      private$..meta$system$version <- Sys.info()[["version"]]
      private$..meta$system$accessed <- Sys.time()
    },

    # Updates system metadata when object has been modified
    modified = function() {
      private$..meta$system$user <- Sys.info()[["user"]]
      private$..meta$system$machine <- Sys.info()[["machine"]]
      private$..meta$system$os <- Sys.info()[["sysname"]]
      private$..meta$system$release <- Sys.info()[["release"]]
      private$..meta$system$version <- Sys.info()[["version"]]
      private$..meta$system$accessed <- Sys.time()
      private$..meta$system$modified <- Sys.time()
    },

    # Updates system metadata when object has been created
    created = function() {
      private$..meta$system$user <- Sys.info()[["user"]]
      private$..meta$system$machine <- Sys.info()[["machine"]]
      private$..meta$system$os <- Sys.info()[["sysname"]]
      private$..meta$system$release <- Sys.info()[["release"]]
      private$..meta$system$version <- Sys.info()[["version"]]
      private$..meta$system$created <- Sys.time()
      private$..meta$system$accessed <- Sys.time()
      private$..meta$system$modified <- Sys.time()
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summaryObjMeta = function(quiet = FALSE) {

      meta <- as.data.frame(private$..meta$object, stringsAsFactors = FALSE,
                            row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\nObject Id    : ", private$..meta$object$id))
        cat(paste0("\nObject Class : ", private$..meta$object$class))
        cat(paste0("\nObject Name  : ", private$..meta$object$name))
        cat(paste0("\nDescription  : ", private$..meta$object$description))

        otherMeta <- meta %>% select(-id, -class, -name, -description)
        if (ncol(otherMeta) > 0) {
          cat("\n\nObject Metadata:\n")
          print(otherMeta, row.names = FALSE)
        }
      }
      return(meta)
    },

    summarySysInfo = function(quiet = FALSE) {

      sys <- as.data.frame(private$..meta$system, stringsAsFactors = FALSE,
                           row.names = NULL)
      if (quiet == FALSE) {
        if (ncol(sys) > 0) {
          cat("\nSystem Information: \n")
          print(sys, row.names = FALSE)
        }
      }
      return(sys)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Basic Get  Methods                            #
    #-------------------------------------------------------------------------#
    getId = function() private$..meta$object$id,

    #-------------------------------------------------------------------------#
    #                           Metadata Method                               #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value = NULL) {

      # Validate
      if (!is.null(key)) {
        if ('class' %in% key) {
          private$..state <- "Unable to change the 'class' of an object."
          self$logIt("Warn")
        }
      }
      if (!is.null(key) & !is.null(value) & (length(key) != length(value))) {
        private$..state <- "Non-null key/value pairs must be of equal length"
        self$logIt("Error")
        stop()
      } else if (is.null(key) & (!(is.null(value)))) {
        private$..state <- "Unable to change meta data. No key provided for value."
        self$logIt("Error")
        stop()
      }

      # Return all metadata
      if (missing(key) & missing(value)) {
        metaDataList <- Filter(Negate(is.null), private$..meta)
        metaDataDfs <- lapply(metaDataList, function(m) {
          df <- as.data.frame(m)
          if (nrow(df) > 0 )
            df
        })
        metaDataDfs <- Filter(Negate(is.null), metaDataDfs)
        return(metaDataDfs)

      # Return selected metadata
      } else if (!is.null(key) & missing(value)) {
        keys <- intersect(names(private$..meta$object), key)
        if (length(keys) > 0) {
          return(as.data.frame(private$..meta$object[names(private$..meta$object) %in% keys]))
        } else {
          private$..state <- "Non existent metadata variable"
          self$logIt("Warn")
          return(NULL)
        }

      # Assign / add key value pairs
      } else {
        for (i in 1:length(key)) {
          private$..meta$object[[key[i]]] <- value[i]
        }
      }
      invisible(self)
    }
  )
)
