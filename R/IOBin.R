#==============================================================================#
#                                      IOBin                                   #
#==============================================================================#
#' IOBin
#'
#'
#' \code{IOBin} Class responsible for reading and writing binary files.
#'
#' @template ioStrategyMethods
#'
#' @template ioStrategyParams
#'
#' @template ioStrategyReturn
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOBin <- R6::R6Class(
  classname = "IOBin",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit =  Entity,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path) {

      private$..logs <- LogR$new()

      fileName <- basename(path)

      if (file.exists(path)) {
        content <- readBin(path, raw(), file.info(path)$size)
        private$..state <- paste0("Successfully read ", fileName, ".")
        private$..meta[["accessed"]] <- Sys.time()
        self$logIt()
      } else {
        private$..state <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
        self$logIt("Error")
        stop()
      }

      return(content)
    },

    write = function(path, content) {

      private$..logs <- LogR$new()

      fileName <- basename(path)
      dirName <- dirname(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      writeBin(content, path)

      private$..state <- paste0("Successfully wrote ", fileName, ".")
      self$logIt()

      invisible(self)
    }
  )
)
