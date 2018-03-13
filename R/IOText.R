#==============================================================================#
#                                      IOText                                  #
#==============================================================================#
#' IOText
#'
#' \code{IOText} Class responsible for reading and writing text files.
#'
#' @usage IOText$new()$read(path = path)
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{read(path)}}{Read method.}
#'   \item{\code{write(path, content)}}{Write method.}
#' }
#'
#' @param path Character string containing the relative file path
#' @param content Character vector to be written to file
#'
#' @return Character string if read method is called. The write method
#' returns TRUE if the write was sucessful, FALSE otherwise.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOText <- R6::R6Class(
  classname = "IOText",
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
        con <- file(path)
        on.exit(close(con))
        content <- readLines(con)
        private$..state <- paste0("Successfully read ", fileName, ".")
        self$logIt()
      } else {
        private$..state <- paste0('Unable to read ', path, '. ',
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

      con <- file(path)
      on.exit(close(con))
      writeLines(content, con)

      private$..state <- paste0("Successfully wrote ", fileName, ".")
      self$logIt()

      invisible(self)
    }
  )
)
