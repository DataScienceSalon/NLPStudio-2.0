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
#'   \item{\code{desc()}}{Active binding getter/setter for object description.}
#'   \item{\code{getName()}}{Method for retrieving an object's name.}
#'   \item{\code{getClassName()}}{Method for retrieving an object's class name}
#'   \item{\code{getPath()}}{Method for retrieving an object's path.}
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
    ..id = character(),
    ..meta = list(
      object = list(),
      app = list(),
      system = list()
    ),
    ..params = list(),
    ..className = character(),
    ..methodName = character(),
    ..state = character(),
    ..logs = character(),

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


    # Creates unique identifier for all descendent classes and objects
    createId = function() {
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      hashid <- hashids::encode(as.integer(private$..meta$system$created) * 1000, settings)
      id <- toupper(hashid)
      return(id)
    },

    # Validates constructor input parameters
    validateParams = function(what = "init") {
      # Valid values are c("init", "associate")

      if (what == "init") {
        private$..methodName <- "initialize"
        v <- Validator$new()
        status <- v$init(self)
        if (status$code == FALSE) {
          private$..state <- status$msg
          self$logIt("Error")
        }
      } else if (what == "attach") {
        private$..methodName <- "attach"
        v <- Validator$new()
        status <- v$attach(self)
        if (status$code == FALSE) {
          private$..state <- status$msg
          self$logIt("Error")
        }
      }  else if (what == "detach") {
        private$..methodName <- "detach"
        v <- Validator$new()
        status <- v$detach(self)
        if (status$code == FALSE) {
          private$..state <- status$msg
          self$logIt("Error")
        }
      }
      return(status)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Basic Get  Methods                            #
    #-------------------------------------------------------------------------#
    getName = function() private$..meta$object$name,
    getId = function() private$..id,
    getParams = function() private$..params,

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, io = NULL) {

      private$..methodName <- 'read'

      # Update text file metadata
      private$..meta$app$filePath <- path
      private$..meta$app$fileName <- basename(path)

      # Read content
      if (is.null(io))  io <- IOFactory$new(private$..meta$app$filePath)$getIOStrategy()
      private$..content <- io$read(path = private$..meta$app$filePath)

      # Update log and system metadata
      private$..state <- paste0("Read Text id ", private$..id, " from ",
                                private$..meta$app$filePath, ".")
      self$logIt()
      private$modified()

      invisible(self)
    },

    write = function(path, io = NULL) {

      private$..methodName <- 'write'

      # Update text file metadata
      private$..meta$app$filePath <- path
      private$..meta$app$fileName <- basename(path)(path)

      # Write text file
      if (is.null(io))  io <- IOFactory$new(private$..meta$app$filePath)$getIOStrategy()
      io$write(path = private$..meta$app$filePath, content = private$..content)

      # Update log
      private$..state <- paste0("Saved Text id ", private$..meta["id"], " to ", path, ". ")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Metadata Method                               #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value = NULL) {

      # Validate
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
      if (is.null(key) & is.null(value)) {
        metaDataList <- Filter(Negate(is.null), private$..meta)
        metaDataDfs <- lapply(metaDataList, function(m) {
          df <- as.data.frame(m)
          if (nrow(df) > 0 )
            df
        })
        metaDataDfs <- Filter(Negate(is.null), metaDataDfs)
        return(metaDataDfs)

      # Return selected metadata
      } else if (!is.null(key) & is.null(value)) {
        keys <- intersect(names(private$..meta$object), key)
        if (length(keys) > 0) {
          return(as.data.frame(private$..meta$object[names(private$..meta$object) %in% keys]))
        } else {
          private$..state <- "Non existent metadata variable"
          self$logIt("Warn")
        }

      # Assign / add key value pairs
      } else {
        for (i in 1:length(key)) {
          private$..meta$object[[key[i]]] <- value[i]
        }
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    }
  )
)
