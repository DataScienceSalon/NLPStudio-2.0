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
#' @param x Character vectors containing actual content of the Text object
#' or the path to a file containing the text content
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
#' avalancheText <- Text$new(name = 'skiReport', content = avalanche)
#'
#' avalancheText <- myText$meta(key = c("author", "editor", "year"),
#'                       value = c("Dorfmeister", "Huffington", "2018"))
#'
#' avalancheText$meta()
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
                 private$..meta$system$modified) {
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
                          created = private$..meta$system$created,
                          user = private$..meta$system$user,
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      return(short)
    },

    initContent = function(x) {
      if (length(x) == 1) {
        io <- IOFactory$new(x)$getIOStrategy()
        content <- io$read(x)
        private$..content <- private$compress(content)
      } else {
        private$..content <- private$compress(x)
      }
      return(TRUE)
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
    initialize = function(name, x) {

      # Initiate logging
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain, validate, then clear parameter list
      private$..params$name <- name
      private$..params$x <- x
      if (private$validateParams()$code == FALSE) stop()
      private$..params <- list()

      # Complete standard initiation tasks, then add content
      private$init(name)

      # Initialize content
      private$initContent(x)

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
