#' TermFreqQ
#'
#' \code{TermFreqQ} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqStrategyQ}} class.
#'
#' TermFreqQ objects are wrappers for the \code{\link[quanteda]{dfm}} class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqQ class.}
#'   \item{\code{get()}}{Returns the quanteda dfm object.}
#'   }
#' @template entityMethods
#'
#' @param x A quanteda dfm object.
#' @template entityParams
#' @template metaParams
#'
#' @return The get method returns a quanteda dfm object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqQ <- R6::R6Class(
  classname = "TermFreqQ",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataCollection0,

  private = list(
    ..dfm = character()
  ),

  active = list(
    dfm = function(value) {
      if (missing(value)) {
        private$..dfm
      } else {
        if (quanteda::is.dfm(value)) {
          private$..dfm <- value
        } else {
          event <- "Parameter is not a valid dfm object."
          private$logR$log(cls = class(self)[1], event = event, "Error")
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL, corpusId = NULL) {

      private$loadDependencies(name = name)

      private$coreMeta(name = name,
                       type = "quanteda dfm",
                       corpusId = corpusId)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqQ(self)
    }
  )
)
