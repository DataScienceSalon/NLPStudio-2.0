#==============================================================================#
#                               SplitCorpus                                    #
#==============================================================================#
#' SplitCorpus
#'
#' \code{SplitCorpus} Splits corpus into training, test and optionally, validation sets
#'
#' Class reshapes Corpus object text into sentence vectors.
#'
#' @usage SplitCorpus$new(x, splits = c(.7, .15, .15),
#'                        setNames =  c("Train", "Validation", "Test"),
#'                        seed = 232)$execute()
#'
#' @template textStudioParams
#' @param splits Numeric vector of splits.
#' @param setNames Character string vector, of equal length to the splits vector,
#' containing the names to which the cross-validation sets will be assigned.
#' @param seed Numeric seed used by the sample function.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextStudio Classes
#' @export
SplitCorpus <- R6::R6Class(
  classname = "SplitCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split0,

  public = list(

    initialize = function(x, name = NULL, splits, setNames = NULL, seed = NULL) {

      private$..className <- "SplitCorpus"
      private$..methodName <- "initialize"
      private$..logs <- LogR$new()

      private$..params <- list()
      private$..params$x <- x
      private$..params$splits <- splits
      private$..params$setNames <- setNames

      if (private$validateParams()$code == FALSE) stop()

      private$..x <- x
      private$..splits <- splits
      private$..setNames <- setNames
      private$..seed <- seed

      # Create Cross-Validation Group Corpus
      private$..cvCorpora <- Corpus$new(name = name)

      # Initialize individual cross-validation sets
      private$initCvSets()

      # log
      private$..event <- paste0("Successfully initialized SplitCorpus class object.")
      private$logIt()

      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Split Documents
      docs <- private$..x$getDocument()
      docSplits <- lapply(docs, function(d) {
        docSplit <- SplitDocument$new(x =  d,
                                   splits = private$..splits,
                                   setNames = private$..setNames,
                                   seed = private$..seed)$execute()
      })


      # Combine documents into relevant cross-validation sets
      for (i in 1:length(private$..cvSets)) {
        for (j in 1:length(docSplits)) {
          private$..cvSets[[i]]$attach(docSplits[[j]][[i]])
        }
        private$..cvCorpora$attach(private$..cvSets[[i]])
      }

      # log
      private$..event <- paste0("Successfully performed SplitCorpus.")
      private$logIt()

      return(private$..cvCorpora)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$splitCorpus(self)
    }
  )
)
