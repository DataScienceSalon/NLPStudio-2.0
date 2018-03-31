#' TokensDocument
#'
#' \code{TokensDocument} Domain class containing a tokenized Document object.
#'
#' Class containing document level text tokenized via the Tokenize class. Objects are aggregated
#' at the corpus level via the Tokens class.
#'
#' @usage myTokensDocument <- TokensDocument$new(x = tokenizedText, name = "skiReport")
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x = NULL, name = NULL)}}{Initializes an object of the TokensDocument class.}
#'   \item{\code{content(value)}}{Active binding method for updating TokensDocument content.}
#'   \item{\code{summary(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
#'   quiet = FALSE, abbreviated = FALSE)}}{Summarizes TokensDocument object.}
#'  }
#' @template entityMethods
#'
#' @param x Character vector or vectors containing tokenized text.
#' @param name Character string containing the name for the TokensDocument object.
#' @template metaParams
#' @template summaryParams
#'
#' @return TokensDocument object, containing the tokenized text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @export
TokensDocument <- R6::R6Class(
  classname = "TokensDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..what = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    oneLiner = function() {

      private$stats()

      if (private$..what %in% c("character", "c")) {

        short <- data.frame(class = private$..meta$core$class,
                            id = private$..meta$core$id,
                            name = private$..meta$core$name,
                            description = private$..meta$core$description,
                            characters = private$..meta$stats$characters,
                            created = private$..meta$state$created,
                            user = private$..meta$system$user,
                            stringsAsFactors = FALSE,
                            row.names = NULL)
      } else if (private$..what %in% c("word", "w")){
        short <- data.frame(class = private$..meta$core$class,
                            id = private$..meta$core$id,
                            name = private$..meta$core$name,
                            description = private$..meta$core$description,
                            words = private$..meta$stats$words,
                            types = private$..meta$stats$types,
                            created = private$..meta$state$created,
                            user = private$..meta$system$user,
                            stringsAsFactors = FALSE,
                            row.names = NULL)
      } else {
        short <- data.frame(class = private$..meta$core$class,
                            id = private$..meta$core$id,
                            name = private$..meta$core$name,
                            description = private$..meta$core$description,
                            sentences = private$..meta$stats$sentences,
                            words = private$..meta$stats$words,
                            types = private$..meta$stats$types,
                            created = private$..meta$state$created,
                            user = private$..meta$system$user,
                            stringsAsFactors = FALSE,
                            row.names = NULL)
      }
      return(short)
    },

    stats = function() {

      getStats = function() {
        content <- private$..content
        private$..meta$stats <- list()

        if (private$..what %in% c("character", "c")) {
          private$..meta$stats$chars <- sum(quanteda::nchar(content))
        } else if (private$..what %in% c("word", "w")) {
          private$..meta$stats$words <- sum(quanteda::ntoken(content))
          private$..meta$stats$types <- sum(quanteda::ntype(tolower(content)))
          private$..meta$stats$chars <- sum(quanteda::nchar(content))
          private$..meta$stats$averageWordLength <- private$..meta$stats$tokens /
            private$..meta$stats$chars
        } else {
          private$..meta$stats$sentences <- sum(quanteda::nsentence(content))
          private$..meta$stats$words <- sum(quanteda::ntoken(content))
          private$..meta$stats$types <- sum(quanteda::ntype(tolower(content)))
          private$..meta$stats$chars <- sum(quanteda::nchar(content))
          private$..meta$stats$averageSentenceLength <-  private$..meta$stats$sentences /
            private$..meta$stats$tokens
          private$..meta$stats$averageWordLength <- private$..meta$stats$tokens /
            private$..meta$stats$chars
        }

        private$..meta$stats$created <- Sys.time()
      }

      if (is.null(private$..meta$stats)) {
        return(getStats())
      } else if (is.null(private$..meta$stats$created)) {
        return(getStats())
      } else if (private$..meta$stats$created <
                 private$..meta$state$modified) {
        return(getStats())
      }
    },

    summaryStats = function(quiet = FALSE) {

      private$stats()

      if (private$..what %in% c("character", "c")) {
        stats <- data.frame(characters = private$..meta$stats$chars)
      } else if (private$..what %in% c("word", "w")){
        stats <- data.frame(sentences = private$..meta$stats$sentences,
                            words = private$..meta$stats$words,
                            types = private$..meta$stats$types,
                            characters = private$..meta$stats$chars,
                            averageWordLength =
                              private$..meta$stats$averageWordLength,
                            row.names = NULL, stringsAsFactors = FALSE)
      } else {
        stats <- data.frame(sentences = private$..meta$stats$sentences,
                            words = private$..meta$stats$tokens,
                            types = private$..meta$stats$types,
                            characters = private$..meta$stats$chars,
                            averageSentenceLength =
                              private$..meta$stats$averageSentenceLength,
                            averageWordLength =
                              private$..meta$stats$averageWordLength,
                            row.names = NULL, stringsAsFactors = FALSE)
      }

      if (quiet == FALSE) {
        if (ncol(stats) > 0) {
          cat("\n\nDescriptive Statistics:\n")
          print(stats, row.names = FALSE)
        }
      }
      return(stats)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL, what = 'word') {

      private$loadDependencies(name = name)

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      private$..params$discrete$variables <- c('what')
      private$..params$discrete$values <- c(what)
      private$..params$discrete$valid <- list(c('sentence', 'word', 'char', 's', 'w', 'c'))
      if (private$validate()$code == FALSE) stop()

      private$..content <- x
      private$..what <- what

      # Initialize metadata and log
      private$initMeta(name = name)
      private$logR$log(cls = class(self)[1], event = "Initialized.")

      invisible(self)
    },

    getTokens = function() {
      private$..content
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
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokensDocument(self)
    }
  )
)
