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
#' @usage myText <- Text$new(name = "skiReport", x = avalanche)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Text class.}
#'  }
#' @template entityMethods
#'
#' @param x Character vector containing text
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
#' avalancheText <- Text$new(name = 'skiReport', content = avalanche)
#'
#' avalancheText <- myText$meta(key = c("author", "editor", "year"),
#'                       value = c("Dorfmeister", "Huffington", "2018"))
#'
#' avalancheText$meta()
#'
#' news <- "./data/en_US.news.txt"
#' newsText <- Text$new(name = 'news', x = news)
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

    stats = function() {

      getStats = function() {
        content <- private$decompress(private$..content)
        private$..meta$stats <- list()
        private$..meta$stats$sentences <- sum(quanteda::nsentence(content))
        private$..meta$stats$tokens <- sum(quanteda::ntoken(content))
        private$..meta$stats$types <- sum(quanteda::ntype(tolower(content)))
        private$..meta$stats$created <- Sys.time()
      }

      if (is.null(private$..meta$stats)) {
        return(getStats())
      } else if (is.null(private$..meta$stats$created)) {
        return(getStats())
      } else if (private$..meta$stats$created <
                 private$..meta$app$modified) {
        return(getStats())
      }
    },

    summaryStats = function(quiet = FALSE) {

      private$stats()

      stats <- as.data.frame(private$..meta$stats, stringsAsFactors = FALSE,
                             row.names = NULL)
      stats <- stats %>% select(-created)
      if (quiet == FALSE) {
        if (ncol(stats) > 0) {
          cat("\n\nDescriptive Statistics:\n")
          print(stats, row.names = FALSE)
        }
      }
      return(stats)
    },

    summaryShort = function() {

      private$stats()

      short <- data.frame(class = private$..meta$object$class,
                          id = private$..meta$object$id,
                          name = private$..meta$object$name,
                          desc = private$..meta$object$desc,
                          sentences = private$..meta$stats$sentences,
                          tokens = private$..meta$stats$tokens,
                          types = private$..meta$stats$types,
                          created = private$..meta$app$created,
                          user = private$..meta$system$user,
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      return(short)
    }
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$accessed()
        return(private$decompress(private$..content))

      } else {
        if (!("character" %in% class(value))) {
          private$..action <- "Text must be of the 'character' class."
          private$logIt("Error")
          stop()
        } else {

          private$..content <- private$compress(value)
          private$modified()
          private$..action <- "Updated text content."
          private$logIt()
        }
      }
      invisible(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x = NULL, name = NULL) {

      # Initiate logging
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()
      private$..params <- list()

      # Compress and store content
      if (!is.null(x)) {
        private$..content <- private$compress(x)
      }

      # Complete standard initiation tasks
      private$init(name)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(meta = TRUE, stats = TRUE, system = TRUE,
                       quiet = FALSE, abbreviated = FALSE) {
      if (abbreviated) {
        result <- private$summaryShort()
      } else {
        result <- list()
        section <- character()

        if (meta) {
          result$meta <- private$summaryObjMeta(quiet = quiet)
          section <- c(section, "Metadata")
        }

        if (stats) {
          result$stats <- private$summaryStats(quiet = quiet)
          section <- c(section, "Descriptive Statistics")
        }

        if (system) {
          result$sys <- private$summarySysInfo(quiet = quiet)
          section <- c(section, "System Info")
        }

        names(result) <- section
      }

      private$accessed()

      invisible(result)
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, repair = FALSE) {

      # Validate parameter
      private$..params <- list()
      private$..params$path <- path
      if (private$validateParams()$code == FALSE) stop()

      private$..methodName <- 'read'
      content <- IO$new()$read(path = path, repair = repair)
      private$..content <- private$compress(content)
      private$modified()

      return(content)
    },

    write = function(path) {

      private$..methodName <- 'write'

      # Decompress, then write text file
      content <- private$decompress(private$..content)
      IO$new()$write(path = path, content = content)

      private$accessed

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
