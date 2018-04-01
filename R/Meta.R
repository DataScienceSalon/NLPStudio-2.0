#' Meta
#'
#' \code{Meta} Contains metadata and methods for maintaining and reporting them.
#'
#' Contains object metadata and the methods for adding, removing, and reporting
#' metadata.  Also includes convenience methods for updating state metadata
#' relating to datetimes of creating, update, and access.
#'
#' @section Meta methods:
#'  \itemize{
#'   \item{\code{new()}}{Intantiates an instance of the Meta class.}
#'   \item{\code{set(key, value)}}{Adds metadata via key value pairs.}
#'   \item{\code{get(key)}}{Returns metadata associated with the key parameter.}
#'   \item{\code{created()}}{Sets state datetime parameters when an object is created.}
#'   \item{\code{modified()}}{Sets state datetime parameters when an object is modified.}
#'   \item{\code{accessed()}}{Sets state datetime parameters when an object is accessed.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..core = list(),
    ..stats = list(),
    ..state = list(),
    ..system = list(),

    logR = character()
  ),

  public = list(

    initialize = function() {

      private$logR <- LogR$new()

      invisible(self)
    },

    setCore = function(key, value) {

      for (i in 1:length(key)) {
        if (key[i] == "id") stop("Unable to set the id variable.")
        private$..core[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    setStats = function(key, value) {

      for (i in 1:length(key)) {
        private$..stats[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    get = function(key = NULL) {

      if (is.null(key)) {
        metaDataList <- Filter(Negate(is.null), list(private$..core,
                                                     private$..stats,
                                                     private$..state,
                                                     private$..system))
        metaDataDfs <- lapply(metaDataList, function(m) {
          df <- as.data.frame(m, stringsAsFactors = FALSE)
          if (nrow(df) > 0 ) {
            df
          }
        })
        names(metaDataDfs) <- c("core", "stats", "state", "system")
        metaDataDfs <- Filter(Negate(is.null), metaDataDfs)
        return(metaDataDfs)

      } else if (!is.null(key)) {

        # Check object level metadata
        keys <- intersect(names(private$..core), key)
        if (length(keys) > 0) {
          return(as.data.frame(private$..core[names(private$..core) %in% keys],
                               stringsAsFactors = FALSE))
        } else {

          # Check stats meta data
          keys <- intersect(names(private$..stats), key)
          if (length(keys) > 0) {
            return(as.data.frame(private$..stats[names(private$..stats) %in% keys],
                                 stringsAsFactors = FALSE))
          } else {

            # Check state level metadata
            keys <- intersect(names(private$..state), key)
            if (length(keys) > 0) {
              return(as.data.frame(private$..state[names(private$..state) %in% keys],
                                   stringsAsFactors = FALSE))
            } else {

              # Check system meta data
              keys <- intersect(names(private$..system), key)
              if (length(keys) > 0) {
                return(as.data.frame(private$..system[names(private$..system) %in% keys],
                                     stringsAsFactors = FALSE))
              } else {
                event <- "Non existent metadata variable"
                private$logR$log(cls = class(self)[1], event = event, level = "Error")
              }
            }
          }
        }
      }
    },

    getName = function() { private$..core$name },
    getId = function() { private$..core$id},

    created = function(id, name, cls, type = NULL, description) {

      # Format core metadata
      private$..core$id <- id
      private$..core$name <- name
      private$..core$class <- cls
      private$..core$type <- type
      private$..core$description <- description

      # Create state metadata
      private$..state$user <- Sys.info()[['user']]
      private$..state$current <- paste0("Instantiated")
      private$..state$created <- Sys.time()

      # Create system meta data
      private$..system$creator <- Sys.info()[["user"]]
      private$..system$machine <- Sys.info()[["machine"]]
      private$..system$os <- Sys.info()[["sysname"]]
      private$..system$release <- Sys.info()[["release"]]
      private$..system$version <- Sys.info()[["version"]]
      invisible(self)
    },

    modified = function() {
      private$..state$user <- Sys.info()[["user"]]
      private$..state$current <- "Modified"
      private$..state$modified <- Sys.time()
      private$..system$machine <- Sys.info()[["machine"]]
      private$..system$os <- Sys.info()[["sysname"]]
      private$..system$release <- Sys.info()[["release"]]
      private$..system$version <- Sys.info()[["version"]]
      invisible(self)
    },

    accessed = function() {
      private$..state$user <- Sys.info()[["user"]]
      private$..state$current <- "Modified"
      private$..state$accessed <- Sys.time()
      private$..system$machine <- Sys.info()[["machine"]]
      private$..system$os <- Sys.info()[["sysname"]]
      private$..system$release <- Sys.info()[["release"]]
      private$..system$version <- Sys.info()[["version"]]
      invisible(self)
    }
  )
)
