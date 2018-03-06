#' Text
#'
#' \code{Text} Class for creating, managing, reading and writing text content and metadata.
#'
#' Class contains text content and metadata for the Text object.
#' The object is instantiated with a name and a character
#' vector containing the text. Once the object is created, users may
#' add, change and remove metadata via key / value pairs. IO
#' methods support reading and writing the texts in a variety
#' of formats using the \code{\link[NLPStudio]{IOStrategy}} factory
#' class.
#'
#' @usage myText <- Text$new(name = "skiReport", content = avalanche)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Text class.}
#'  }
#' @template entityMethods
#'
#' @param content Character vectors containing actual content of the Text object.
#' @template ioParams
#' @template metaParams
#'
#' @return Text object, containing the text content, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' avalanche <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' myText <- Text$new(name = 'skiReport', content = avalanche)
#'
#' myText <- myText$meta(key = c("author", "editor", "year"),
#'                       value = c("Dorfmeister", "Huffington", "2018"))
#'
#' myText$meta()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
Text <- R6::R6Class(
  classname = "Text",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character(),

    stats = function(content) {
      private$..meta$stats <- list()
      private$..meta$stats$Sentences <- sum(quanteda::nsentence(content))
      private$..meta$stats$Tokens <- sum(quanteda::ntoken(content))
      private$..meta$stats$Types <- sum(quanteda::ntype(content))
      private$..meta$stats$Chars <- sum(nchar(content))
      private$..meta$stats$`Ave Word Len` <- private$..meta$stats$Chars /
                                         private$..meta$stats$Tokens
      private$..meta$stats$`Ave Sent Len` <- private$..meta$stats$Tokens /
                                         private$..meta$stats$Sentences
    }
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$accessed()
        txt <- strsplit(memDecompress(private$..content, "g",
                                      asChar = TRUE), "\n")[[1]]
        return(txt)

      } else {
        if (!("character" %in% class(value))) {
          private$..state <- "Text must be of the 'character' class."
          self$logIt("Error")
          stop()
        } else {

          private$..content <- memCompress(value, "g")
          private$modified()
          private$..state <- "Updated text content."
          self$logIt()
        }
      }
      invisible(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, content) {

      # Initiate logging
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params <- as.list(match.call())
      if (private$validateParams()$code == FALSE) stop()
      private$..params <- list()

      # Obtain descriptive statistics, compress content, and wrap up instantiation
      private$stats(content)
      private$..content <- memCompress(content, "g")
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summaryStats = function(quiet = FALSE) {
      stats <- as.data.frame(private$..meta$stats, stringsAsFactors = FALSE,
                             row.names = NULL)
      if (quiet == FALSE) {
        if (ncol(stats) > 0) {
          cat("\n\nDescriptive Statistics:\n")
          colnames(stats) <- sapply(colnames(stats), proper)
          print(stats, row.names = FALSE)
        }
      }
      return(stats)
    },

    summary = function(quiet = FALSE) {
      result <- list()
      result$meta <- self$summaryObjMeta(quiet = quiet)
      result$stats <- self$summaryStats(quiet = quiet)
      result$sys <- self$summarySysMeta(quiet = quiet)
      names(result) <- c("Metadata", "Descriptive Statistics", "System Info")
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$text(self)
    }
  )
)
