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
  inherit = Base,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path) {

      private$..logs <- LogR$new()

      fileName <- basename(path)

      if (file.exists(path)) {
        content <- readBin(path, raw(), file.info(path)$size)
        private$..action <- paste0("Successfully read ", fileName, ".")
        private$logIt()
      } else {
        private$..action <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
        private$logIt("Error")
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

      private$..action <- paste0("Successfully wrote ", fileName, ".")
      private$logIt()

      invisible(self)
    }
  )
)
