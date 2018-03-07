#' Text
#'
#' \code{Text} Class for creating, managing, reading and writing text content and metadata.
#'
#' Class contains text content and metadata for the Text object.
#' The object is instantiated with a name and a character
#' vector containing the text. Once the object is created, users may
#' add, change and remove metadata via key / value pairs. IO
#' methods support reading and writing the texts in a variety
#' of formats using the \code{\link[NLPStudio]{IOFactory}} factory
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

    compress = function(x) {
      memCompress(x, "g")
    },

    decompress = function(x) {
      strsplit(memDecompress(x, "g", asChar = TRUE), "\n")[[1]]
    },

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
    },

    summaryStats = function(quiet = FALSE) {
      stats <- as.data.frame(private$..meta$stats, stringsAsFactors = FALSE,
                             row.names = NULL)
      if (quiet == FALSE) {
        if (ncol(stats) > 0) {
          cat("\nDescriptive Statistics:\n")
          colnames(stats) <- sapply(colnames(stats), proper)
          print(stats, row.names = FALSE)
        }
      }
      return(stats)
    }
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$accessed()
        return(private$decompress(private$..content))

      } else {
        if (!("character" %in% class(value))) {
          private$..state <- "Text must be of the 'character' class."
          self$logIt("Error")
          stop()
        } else {

          private$..content <- private$compress(value)
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
      private$..params$name <- name
      private$..params$content <- content
      if (private$validateParams()$code == FALSE) stop()
      private$..params <- list()

      # Obtain descriptive statistics, compress content, and wrap up instantiation
      private$stats(content)
      private$..content <- memCompress(content, "g")
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                               #
    #-------------------------------------------------------------------------#
    summary = function(meta = TRUE, stats = TRUE, system = TRUE, quiet = FALSE) {
      result <- list()
      if (meta) result$meta <- private$summaryObjMeta(quiet = quiet)
      if (stats) result$stats <- private$summaryStats(quiet = quiet)
      if (system) result$sys <- private$summarySysInfo(quiet = quiet)
      names(result) <- c("Metadata", "Descriptive Statistics", "System Info")
      return(result)
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, io = NULL) {

      private$..methodName <- 'read'

      # Update text file metadata
      private$..meta$object$filePath <- path
      private$..meta$object$fileName <- basename(path)

      # Read and compress content
      if (is.null(io))  io <- IOFactory$new(path)$getIOStrategy()
      content <- io$read(path = path)
      private$..content <- private$compress(content)

      # Update log and system metadata
      private$..state <- paste0("Read ", private$..meta$object$class,
                                " object, '", private$..meta$object$name,
                                "' from ", path, ". ")
      self$logIt()
      private$modified()

      invisible(content)
    },

    write = function(path, io = NULL) {

      private$..methodName <- 'write'

      # Update text file metadata
      private$..meta$object$filePath <- path
      private$..meta$object$fileName <- basename(path)

      # Decompress, then write text file
      content <- private$decompress(private$..content)
      if (is.null(io))  io <- IOFactory$new(path)$getIOStrategy()
      io$write(path = path, content = content)

      # Update log
      private$..state <- paste0("Saved ", private$..meta$object$class,
                                " object, '", private$..meta$object$name,
                                "' to ", path, ". ")
      private$accessed
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$text(self)
    }
  )
)
