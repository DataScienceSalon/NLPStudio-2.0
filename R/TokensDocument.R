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
#'   \item{\code{new(x, what = c("sentence", "word", "character"), name = NULL)}}{Initializes an object of the TokensDocument class.}
#'   \item{\code{content(value)}}{Active binding method for updating TokensDocument content.}
#'   \item{\code{summary(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
#'   quiet = FALSE, abbreviated = FALSE)}}{Summarizes TokensDocument object.}
#'  }
#' @template entityMethods
#'
#' @param x Character vector or vectors containing tokenized text.
#' @param what Character string specifying the level of tokenizization.
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
  inherit = DataDocument0,

  private = list(
    ..what = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    oneLiner = function(meta, quiet = FALSE) {

      if (private$..what %in% c("sentence")) {

        df <- data.frame(class = meta$core$class,
                         id = meta$core$id,
                         textId = meta$core$textId,
                         type = meta$core$type,
                         name = meta$core$name,
                         description = meta$core$description,
                         sentences = meta$stats$sentences,
                         words = meta$stats$words,
                         types = meta$stats$types,
                         characters = meta$stats$characters,
                         averageSentenceLength = meta$stats$averageSentenceLength,
                         averageWordLength = meta$stats$averageWordLength,
                         created = meta$state$created,
                         creator = meta$system$creator,
                         stringsAsFactors = FALSE,
                         row.names = NULL)

      } else if (private$..what %in% c("word")) {
        df <- data.frame(class = meta$core$class,
                         id = meta$core$id,
                         textId = meta$core$textId,
                         type = meta$core$type,
                         name = meta$core$name,
                         description = meta$core$description,
                         words = meta$stats$words,
                         types = meta$stats$types,
                         characters = meta$stats$characters,
                         averageWordLength = meta$stats$averageWordLength,
                         created = meta$state$created,
                         creator = meta$system$creator,
                         stringsAsFactors = FALSE,
                         row.names = NULL)
      } else {
        df <- data.frame(class = meta$core$class,
                         id = meta$core$id,
                         textId = meta$core$textId,
                         type = meta$core$type,
                         name = meta$core$name,
                         description = meta$core$description,
                         characters = meta$stats$characters,
                         created = meta$state$created,
                         creator = meta$system$creator,
                         stringsAsFactors = FALSE,
                         row.names = NULL)
      }
      return(df)
    },

    core = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$core, stringsAsFactors = FALSE,
                          row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\n\nObject Id    : ", meta$core$id))
        cat(paste0("\n     Text Id : ", meta$core$textId))
        cat(paste0("\nObject Class : ", meta$core$class))
        cat(paste0("\n Object Type : ", meta$core$type))
        cat(paste0("\n Object Name : ", meta$core$name))
        cat(paste0("\n Description : ", meta$core$description))

        otherMeta <- df %>% select(-id, -textId, -class, -type, -name,
                                   -description)
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

      content <- private$..content
      stats <- list()

      # Obtain statistics
      if (private$..what %in% c("character")) {
        stats$characters <- sum(nchar(content))
      } else if (private$..what %in% c("word")) {
        stats$words <- sum(quanteda::ntoken(content))
        stats$types <- sum(quanteda::ntype(tolower(content)))
        stats$characters <- sum(nchar(content))
        stats$averageWordLength <- stats$characters / stats$words
      } else {
        stats$sentences <- sum(quanteda::nsentence(content))
        stats$words <- sum(quanteda::ntoken(content))
        stats$types <- sum(quanteda::ntype(tolower(content)))
        stats$characters <- sum(nchar(content))
        stats$averageSentenceLength <- stats$words / stats$sentences
        stats$averageWordLength <- stats$characters / stats$words
      }
      stats$created <- Sys.time()

      # Format metadata
      keys <- names(stats)
      for (i in 1:length(stats)) {
        private$meta$setStats(key = keys[i], value = stats[[i]])
      }

      invisible(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, textId, what = 'word', name = NULL) {

      private$loadDependencies()

      # Obtain, validate, then clear parameter list
      private$..params <- list()
      private$..params$x <- x
      private$..params$discrete$variables <- c('what')
      private$..params$discrete$values <- c(what)
      private$..params$discrete$valid <- list(c('sentence', 'word', 'character'))
      if (private$validate()$code == FALSE) stop()

      # Initialize private members, metadata and log
      private$..content <- x
      private$..what <- what
      private$coreMeta(name = name,
                       type = paste0(proper(what), " Tokens"),
                       textId = textId)
      private$statsMeta()
      private$logR$log(cls = class(self)[1], event = "Initialized.")

      invisible(self)
    },

    getTokens = function() {
      private$meta$accessed()
      private$..content
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, stats = TRUE, state = TRUE, system = TRUE,
                       quiet = FALSE, abbreviated = FALSE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta,  quiet = quiet)
          section <- c(section, "Core Metadata")
        }

        if (stats) {
          result$stats <- private$summaryStats(meta, quiet = quiet)
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
