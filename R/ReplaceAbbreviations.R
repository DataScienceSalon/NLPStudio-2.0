#------------------------------------------------------------------------------#
#                        Replace Abbreviations                                 #
#------------------------------------------------------------------------------#
#' ReplaceAbbreviations
#'
#' \code{ReplaceAbbreviations} Replaces abbreviations with long form.
#'
#' This is a wrapper for the replace_abbreviations function in the QDAP package.
#' Source \url{https://cran.r-project.org/web/packages/qdap/qdap.pdf}
#'
#' @usage ReplaceAbbreviations$new(x, replace, ignoreCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Should case be ignored? Only applies to default dictionary.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceAbbreviations <- R6::R6Class(
  classname = "ReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..abbreviations = character(),
    ..ignoreCase = logical(),

    processDocument = function(document) {

      content <- document$content

      if (is.null(private$..abbreviations)) {
        document$content <- qdap::replace_abbreviation(text.var = content,
                                              ignore.case = private$..ignoreCase)

      } else {
        if ("data.frame" %in% class(private$..abbreviations)) {
          pattern <- as.character(private$..abbreviations[,1])
          if (ncol(private$..abbreviations) == 2) {
            replacement <- as.character(private$..abbreviations[,2])
          } else if (ncol(private$..abbreviations) == 1) {
            replacement <- as.character(private$..replacement)
          }
        } else {
          pattern <- paste("\\b", private$..abbreviations, "(?!\\w)", sep = "")
          replacement <- private$..replacement
        }

        document$content <- textclean::mgsub(x = content,  pattern = pattern,
                                    replacement = replacement,  fixed = FALSE,
                                    perl = TRUE)
      }
      document <- private$logEvent(document)

      return(document)
    }
  ),

  public = list(
    initialize = function(x, abbreviations = NULL,
                          replacement = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceAbbreviations"
      private$..methodName <- "initialize"
      private$..meta$object$name <- private$..className
      private$..logs  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      private$..params$pattern <- abbreviations
      private$..params$replacement <- replacement
      private$..params$logicals$variables <- c('ignoreCase')
      private$..params$logicals$values <- c(ignoreCase)
      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..abbreviations <- abbreviations
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase

      invisible(self)
    }
  )
)
