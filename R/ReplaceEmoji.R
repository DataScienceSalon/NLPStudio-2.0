#------------------------------------------------------------------------------#
#                         Replace Emoji                                        #
#------------------------------------------------------------------------------#
#' ReplaceEmoji
#'
#' \code{ReplaceEmoji}  Replace emojis with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emoji}} that replaces
#' emojis with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmoji$new(x, emojis = NULL)$execute()
#'
#' @template textStudioParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmoji} Returns a vector with emojis replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoji <- R6::R6Class(
  classname = "ReplaceEmoji",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emojis = data.table(),

    processDocument = function(document) {
      content <- document$content
      if (is.null(private$..emojis)) {
        document$content <- textclean::replace_emoji(x = content)
      } else {
        document$content <- textclean::replace_emoji(x = content,
                                            emoji_dt = private$..emojis)
      }
      document <- private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, emojis = NULL) {
      private$..className <- "ReplaceEmoji"
      private$..methodName <- "initialize"
      private$..meta$core$name <- private$..className
      private$logR  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..emojis <- emojis

      invisible(self)
    }
  )
)
