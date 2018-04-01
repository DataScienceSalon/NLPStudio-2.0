#' TextDocument
#'
#' \code{TextDocument} Class for creating, managing, reading and writing TextDocument content and metadata.
#'
#' Class contains TextDocument content and metadata for the TextDocument object.
#' The object may be instantiated with a name and character
#' vectors containing the TextDocument text. Once the object is created, users may
#' add, change and remove metadata via key / value pairs. IO
#' methods support reading and writing the TextDocuments in a variety
#' of formats using the \code{\link[NLPStudio]{IOFactory}} factory
#' class.
#'
#' @usage myTextDocument <- TextTextDocument$new(name = "skiReport", x = avalanche)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x = NULL, name = NULL)}}{Initializes an object of the TextDocument class.}
#'   \item{\code{content(value)}}{Active binding method for updating TextDocument text content.}
#'   \item{\code{read(path, repair = FALSE)}}{Reads text from a file designated by the path variable.
#'   If repair is set to TRUE, select ASCII control characters are replaced with spaces
#'   using \code{\link[NLPStudio]{RepairFile}} }
#'   \item{\code{write(path)}}{Writes the TextDocument object content to a file in plain text format.}
#'   \item{\code{summary(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
#'   quiet = FALSE, abbreviated = FALSE)}}{Summarizes TextDocument object.}
#'  }
#' @template entityMethods
#'
#' @param x Character vector containing TextDocument
#' @param name Character string containing the name for the TextDocument object.
#' @param repair Logical. If TRUE, the read method will invoke the RepairFile class on the text.
#' @param task Character string containing the name of the TextStudio class associated with the change to the TextDocument content
#' @template metaParams
#' @template summaryParams
#'
#' @return TextDocument object, containing the TextDocument content, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' avalanche <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' avalancheTextDocument <- TextTextDocument$new(x = avalanche, name = 'skiReport')
#'
#' avalancheTextDocument <- myTextTextDocument$meta(key = c("author", "editor", "year"),
#'                       value = c("Dorfmeister", "Huffington", "2018"))
#'
#' avalancheTextTextDocument$meta()
#'
#' news <- "./data/en_US.news.txt"
#' newsTextDocument <- TextTextDocument$new(name = 'news', x = news)
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Core Classes
#' @export
TextDocument <- R6::R6Class(
  classname = "TextDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(

    compress = function(x) {
      memCompress(x, "g")
    },

    decompress = function(x) {
      strsplit(memDecompress(x, "g", asChar = TRUE), "\n")[[1]]
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    oneLiner = function(meta, quiet = FALSE) {

      df <- data.frame(class = meta$core$class,
                       id = meta$core$id,
                       name = meta$core$name,
                       description = meta$core$description,
                       created = meta$state$created,
                       creator = meta$system$creator,
                       stringsAsFactors = FALSE,
                       row.names = NULL)
      return(df)
    },

    core = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$core, stringsAsFactors = FALSE,
                          row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\n\nObject Id    : ", meta$core$id))
        cat(paste0("\nObject Class : ", meta$core$class))
        cat(paste0("\nObject Name  : ", meta$core$name))
        cat(paste0("\nDescription  : ", meta$core$description))

        otherMeta <- df %>% select(-id, -class, -name, -description)
        if (ncol(otherMeta) > 0) {
          cat("\n\nAdditional Core Metadata:\n")
          print(otherMeta, row.names = FALSE)
        }
      }
      return(df)
    },

    #-------------------------------------------------------------------------#
    #                           Statistics Metadata                           #
    #-------------------------------------------------------------------------#
    statsMeta = function() {

      stats <- list()
      content <- private$decompress(private$..content)
      stats$sentences <- sum(quanteda::nsentence(content))
      stats$tokens <- sum(quanteda::ntoken(content))
      stats$types <- sum(quanteda::ntype(tolower(content)))
      stats$created <- Sys.time()

      keys <- names(stats)
      for (i in 1:length(stats)) {
        private$meta$setStats(key = keys[i], value = stats[[i]])
      }

      invisible(self)
    }
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$meta$accessed()
        return(private$decompress(private$..content))

      } else {
        if (!("character" %in% class(value))) {
          event <- "TextDocument content must be of the 'character' class."
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        } else {

          private$..content <- private$compress(value)
          private$statsMeta()
          private$meta$modified()
          event <- "Updated TextDocument content."
          private$logR$log(cls = class(self)[1], event = event)
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

      private$loadDependencies(name = name)

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      if (private$validate()$code == FALSE) stop()


      # Compress and store content
      if (!is.null(x)) {
        private$..content <- private$compress(x)
      }

      # Initialize metadata and log instantiation
      private$coreMeta(name = name)
      if (!is.null(x)) private$statsMeta()
      private$logR$log(cls = class(self)[1], event = "Initialized.")

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
                       quiet = FALSE, abbreviated = FALSE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta, quiet = quiet)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta,  quiet = quiet)
          section <- c(section, "Core Metadata")
        }

        if (stats) {
          result$stats <- private$summaryStats(quiet = quiet)
          section <- c(section, "Descriptive Statistics")
        }

        if (state) {
          result$state <- private$state(meta, quiet = quiet)
          section <- c(section, "State Information")
        }

        if (system) {
          result$sys <- private$system(meta, quiet = quiet)
          section <- c(section, "System Information")
        }

        names(result) <- section
      }

      private$meta$accessed()

      invisible(result)
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, repair = FALSE) {


      private$..methodName <- 'read'

      # Validate parameter
      private$..params <- list()
      private$..params$path <- path
      if (private$validate(what = "read")$code == FALSE) stop()

      content <- IO$new()$read(path = path, repair = repair)
      private$..content <- private$compress(content)
      private$meta$modified()

      return(content)
    },

    write = function(path) {

      private$..methodName <- 'write'

      # Decompress, then write TextDocument file
      content <- private$decompress(private$..content)
      IO$new()$write(path = path, content = content)

      private$meta$accessed

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textDocument(self)
    }
  )
)
