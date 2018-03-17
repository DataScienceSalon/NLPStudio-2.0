#------------------------------------------------------------------------------#
#                                    StripText                                 #
#------------------------------------------------------------------------------#
#' StripText
#'
#' \code{StripText}  Strips text of unwanted characters.
#'
#' A wrapper for \code{\link[qdap]{strip}} Strips text of unwanted characters.
#' Source \url{https://cran.r-project.org/web/packages/qdap/qdap.pdf}
#'
#' @usage StripText$new(x, keepChar = "~~", removeDigit = TRUE,
#'                      removeApostrophe = TRUE, lowerCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param keepChar A character vector of symbols (i.e., punctuation) that strip
#' should keep. The default is to strip every symbol except apostrophes and a double
#' tilde "~~". The double tilde "~~" is included for a convenient means of keeping
#' word groups together in functions that split text apart based on spaces.
#' To remove double tildes "~~" set char.keep to NULL.
#' @param removeDigit Logical.  If TRUE, digits are removed from the text.
#' @param removeApostrophe Logical.  If TRUE, apostrophes are removed from the text.
#' @param lowerCase Logical.  If TRUE, text is converted to lower case.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{StripText} Returns text with unwanted characters removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
StripText <- R6::R6Class(
  classname = "StripText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..keepChar = character(),
    ..removeDigit = logical(),
    ..removeApostrophe = logical(),
    ..lowerCase = logical(),

    processDocument = function(document) {
      document$content <- qdap::strip(x = document$content,
                                      char.keep = private$..keepChar,
                                      digit.remove = private$..removeDigit,
                                      apostrophe.remove = private$..removeApostrophe,
                                      lower.case = private$..lowerCase)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, keepChar = "~~", removeDigit = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$..className <- "StripText"
      private$..methodName <- "initialize"
      private$..logs  <- LogR$new()

      private$..x <- x
      private$..keepChar <- keepChar
      private$..removeDigit <- removeDigit
      private$..removeApostrophe <- removeApostrophe
      private$..lowerCase <- lowerCase
      invisible(self)
    }
  )
)
