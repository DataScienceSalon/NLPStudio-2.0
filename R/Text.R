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

      # Initiate logging variables and system meta data
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Obtain and validate parameters
      private$..params$name <- name
      private$..params$content <- content
      if (private$validateParams()$code == FALSE) stop()

      # Obtain descriptive statistics, create description variable, and compress
      private$stats(content)
      private$..meta$object$description <- paste0(private$..meta$app$class, " object, ",
                                                  "created on ",
                                                  format(Sys.Date(), "%a %b %d %Y"),
                                                  " by ", Sys.info()[['user']], ".")
      private$..params$content <- memCompress(content, "g")

      # Complete Instantiation
      private$init(name)

      invisible(self)
    },

    summary = function() {

      private$..methodName <- "summary"

      # Obtain meta data and descriptive statistics
      df <- data.frame(Id = private$..meta$app$id,
                       Class = private$..meta$app$class,
                       Name = private$..meta$object$name,
                       stringsAsFactors = FALSE)
      stats <- as.data.frame(private$..meta$stats)
      created <- data.frame(Created = private$..meta$system$created,
                               stringsAsFactors = FALSE)
      md <- as.data.frame(private$..meta$object, stringsAsFactors = FALSE) %>%
        select(-name)
      df <- cbind(df, stats, created, md)
      return(df)

    },

    tprint = function() {
      cat("\n#-------------------------------------------------------------------------#")
      cat("\n#                         Text Object Summary                             #")
      cat("\n#-------------------------------------------------------------------------#")
      cat(paste0("\n    Object Id: ", private$..meta$app$id),
                 "           Created: ", private$..meta$system$created)
      cat(paste0("\n  Object Name: ", private$..meta$object$name),
                 "                   User: ", private$..meta$system$user)
      cat(paste0("\n\n  Description: ", private$..meta$object$description))
      cat("\n\n\n                     Descriptive Statistics \n\n")
      print(data.frame(private$..meta$stats), row.names = FALSE)
      cat("\n\n\n                         Object Metadata \n\n")
      print(as.data.frame(private$..meta$object, stringsAsFactors = FALSE) %>%
              filter(-name), row.names = FALSE)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$text(self)
    }
  )
)
