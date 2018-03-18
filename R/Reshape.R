#------------------------------------------------------------------------------#
#                                 Reshape                                      #
#------------------------------------------------------------------------------#
#' Reshape
#'
#' \code{Reshape} Reshapes objects into document or sentence level of aggregation.
#'
#' Class responsible for reshaping Corpus and Document object text into a single
#' document level text vector, or sentence vectors.
#'
#' @usage Reshape$new(x, to = c("document", "sentence"))$execute()
#'
#' @template textStudioParams
#' @param to Character vector indicating the level to which the Corpus or
#' Document object should be reshaped. Valid values are "document" and
#' "sentence".  The starting letters, "d", and "s" may also be used.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{Reshape} Returns a Corpus or Document object, with text
#' reshaped accordingly.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
Reshape <- R6::R6Class(
  classname = "Reshape",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..to = character(),

    processDocument = function(document) {

      if (private$..to %in% c("document", "d")) {
        document$content <- paste(document$content, collapse = "")
      } else {
        reshaped <- Tokenize$new(t1, what = "sentence")
      }
      document$content <- textclean::replace_number(x = document$content,
                                            num.paste = private$..joinNumbers,
                                            remove = private$..remove)
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, joinNumbers = FALSE, remove = FALSE) {
      private$..className <- "Reshape"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      private$..x <- x
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove

      invisible(self)
    }
  )
)
