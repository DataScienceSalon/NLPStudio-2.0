#' VString
#'
#' \code{VString} Class for validating strings
#'
#' This class provide a methods for validating character strings.
#'
#' @param value Character string containing the value to be validated
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
VString <- R6::R6Class(
  "VString",
  public = list(
    initialize = function() invisible(self),
    validate = function(value) {

      if (!("character" %in% class(value)))  return(FALSE)
      if (!exists('value')) return(FALSE)
      if (length(value) != 1) return(FALSE)
      if (is.na(value)) return(FALSE)
      if (is.null(value)) return(FALSE)
      if (is.logical(value)) return(FALSE)
      if (value == "") return(FALSE)
      if (grepl("\\s+", value) == TRUE) return(FALSE)
      return(TRUE)
    }
  )
)

