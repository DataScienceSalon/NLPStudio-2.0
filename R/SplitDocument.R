#==============================================================================#
#                             SplitDocument                                    #
#==============================================================================#
#' SplitDocument
#'
#'
#' \code{SplitDocument} Splits a Document object into cross-validation sets.
#'
#' Splits a Document object into cross validation sets, given document
#' split proportions.
#'
#' @usage SplitDocument$new(x, splits = c(.7, .15, .15),
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
SplitDocument <- R6::R6Class(
  classname = "SplitDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split0,

  private = list(

    spawn = function() {
      Document$new()
    }
  ),

  public = list(
    initialize = function(x, name = NULL, splits, setNames = NULL, seed = NULL) {

      private$..className <- "SplitDocument"
      private$..methodName <- "initialize"
      private$logR <- LogR$new()

      private$..params <- list()
      private$..params$x <- x
      private$..params$splits <- splits
      private$..params$setNames <- setNames

      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..splits <- splits
      private$..setNames <- setNames
      private$..seed <- seed

      # Initialize individual cross-validation sets
      private$initCvSets()

      # log
      event <- paste0("Successfully initialized SplitDocument class object.")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    },

    execute = function() {

      # Obtain content
      content <- private$..x$content

      # Set seed
      if (!is.null(private$..seed)) {
        set.seed(private$..seed)
      }

      # Establish sample indices
      x <- c(1:length(private$..splits))
      ss <- sample(x, size = length(content), replace = TRUE, prob = private$..splits)

      for (i in 1:length(private$..cvSets)) {

        private$..cvSets[[i]]$content <- content[ss == i]
      }
      # log
      event <- paste0("Successfully split Document")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(private$..cvSets)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$splitDocument(self)
    }
  )
)
