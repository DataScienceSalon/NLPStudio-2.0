#' Document0
#'
#' \code{Document0} Abstract class for concrete Document classes.
#'
#' Abstract class that defines common methods for concrete Document classes,
#' such as the TextDocument, TokensDocument, NGramDocument, POSDocument,
#' and StemDocument classes.
#'
#' @template entityMethods
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character(),

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

    summaryStats = function(meta, quiet = FALSE) {

      stats <- as.data.frame(meta$stats, stringsAsFactors = FALSE,
                             row.names = NULL)
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
    initialize = function() {stop("This method is not implemented for this abstract class.")}
  )
)
