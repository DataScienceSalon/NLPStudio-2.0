#------------------------------------------------------------------------------#
#                         Document Frequency Matrix                            #
#------------------------------------------------------------------------------#
#' DFM
#'
#' \code{DFM}  Creates a document frequency matrix from a Corpus or Document object.
#'
#' A wrapper for \code{\link[quanteda]{dfm}}, this classes creates a sparse
#' document frequency matrix for a Corpus or Document object.  The DFM is attached
#' to the object and returned to the calling environment.
#' Source \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#'
#' @usage DFM$new(x tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,
#' #' dictionary = NULL, thesaurus = NULL, valuetype = c("glob", "regex","fixed"),
#' groups = NULL, verbose = quanteda_options("verbose"), ...)$execute()
#'
#' @template dataStudioParams
#' @template dataStudioMethods
#' @template dataStudioClasses
#' @template dataStudioDesign
#'
#' @param tolower if TRUE, converts all letters to lower case.
#' @param stem if TRUE, stem words
#' @param select a pattern of user-supplied features to keep, while excluding all others.
#' See \code{\link[quanteda]{dfm}} for details.
#' @param remove a pattern of user-supplied features to ignore, such as "stop words".
#' See \code{\link[quanteda]{dfm}} for details.
#' @param dictionary a dictionary object to apply to the tokens when creating the dfm.
#' See \code{\link[quanteda]{dfm}} for details.
#' @param thesaurus a dictionary object that will be applied as if exclusive = FALSE.
#' See \code{\link[quanteda]{dfm}} for details.
#' @param valuetype A character string containing the type of pattern matching:
#' "glob" for "glob"-style wildcard expressions; "regex" for regular expressions; or
#' "fixed" for exact matching. Defaults to 'fixed'. See \code{\link[quanteda]{dfm}} for details.
#' @param groups Either: a character vector containing the names of document variables to
#' be used for grouping; or a factor or object that can be coerced into a factor
#' equal in length or rows to the number of documents. See \code{\link[quanteda]{dfm}}
#' for details.
#' @param verbose Display messages if TRUE.
#'
#' @return a \code{\link{DFM}} object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes, NLPStudio Classes
#' @export
DFM <- R6::R6Class(
  classname = "DFM",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(
    ..settings = list(
      tolower = logical(),
      stem = logical(),
      select = character(),
      remove = character(),
      dictionary = character(),
      thesaurus = character(),
      valuetype = character(),
      groups = character(),
      verbose = character()
    ),
    ..data = character(),

    processCorpus = function() {
      dfm <- private$..x$getDFM()

      if (is.null(dfm) |
          (document$meta(key == 'modified') > dfm$meta(key == "created") |
           dfm$settings() != private$..settings)) {
        q <- ConverterQuanteda$new()$convert(private$..x)
        private$..data <- quanteda::dfm(q,
                                        tolower = private$..settings$tolower,
                                        stem = private$..settings$stem,
                                        select = private$..settings$select,
                                        remove = private$..settings$remove,
                                        dictionary = private$..settings$dictionary,
                                        thesaurus = private$..settings$thesaurus,
                                        valuetype = private$..settings$valuetype,
                                        groups = private$..settings$groups,
                                        verbose = private$..settings$verbose)

      }
      return(TRUE)
    }
  ),

  public = list(
    initialize = function(x, tolower = TRUE, stem = FALSE, select = NULL,
                          remove = NULL,  dictionary = NULL, thesaurus = NULL,
                          valuetype = "fixed", groups = NULL,
                          verbose = quanteda::quanteda_options("verbose")) {
      private$..className <- "DFM"
      private$..methodName <- "initialize"
      private$..meta$core$name <- private$..className
      private$logR  <- LogR$new()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c("tolower", "stem")
      private$..params$logicals$values <- c(tolower, stem)
      private$..params$discrete$variables <- list(c("valuetype"))
      private$..params$discrete$values <- list(c(valuetype))
      private$..params$discrete$valid <- list(c("glob", "regex","fixed"))
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..settings$tolower <- tolower
      private$..settings$stem <- stem
      private$..settings$select <- select
      private$..settings$remove <- remove
      private$..settings$dictionary <- dictionary
      private$..settings$thesaurus <- thesaurus
      private$..settings$valuetype <- valuetype
      private$..settings$groups <- groups
      private$..settings$verbose <- verbose

      private$created()

      invisible(self)
    }
  )
)
