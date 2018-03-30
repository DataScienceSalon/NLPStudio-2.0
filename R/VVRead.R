#' VVRead
#'
#' \code{VVRead} Visitor class responsible for validating read methods
#'
#' Validates read methods. Confirms validity of file paths and file types.
#'
#' @usage visitor <- VVRead$new()
#'
#' @section Core Methods:
#' The VVRead methods are as follows:
#'  \itemize{
#'   \item{\code{text(object)}}{Method for validating the read method for Text objects.}
#' }
#'
#' @param object The domain object
#' @return status List containing:
#' \itemize{
#'  \item status Logical TRUE, if validation was passed, FALSE otherwise
#'  \item msg Character string describing the error, if status is FALSE
#' }
#'
#' @examples
#' visitor <- VVRead$new()
#' object$accept(visitor)
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VVRead <- R6::R6Class(
  classname = "VVRead",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validateFile = function(object) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()
      path <- p$path

      if (!R.utils::isFile(path)) {
        status$code <- FALSE
        status$msg <- paste0("File path ", path, " not found.")
        return(status)
      } else if (is.null(IOFactory$new(path)$getIOStrategy())) {
        status$code <- FALSE
        status$msg <- paste0("File ", path, " is an unsupported file type.",
                             "See ?", class(object)[1],
                             " for further assistance.")
        return(status)
      }
      return(status)
    }
  ),
  public = list(

    initialize = function() { invisible(self) },

    document = function(object) {
      return(private$validateFile(object = object))
    }
  )
)
