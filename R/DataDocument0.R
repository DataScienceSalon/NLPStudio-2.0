#' DataDocument0
#'
#' \code{DataDocument0} Abstract class for concrete DataDocument classes.
#'
#' Abstract class that defines common methods for concrete Document classes,
#' such as the TokensDocument, NGramDocument, POSDocument, and StemDocument
#' classes.
#'
#' @template entityMethods
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
DataDocument0 <- R6::R6Class(
  classname = "DataDocument0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(

    #-------------------------------------------------------------------------#
    #                      Initialize Metadata Method                         #
    #-------------------------------------------------------------------------#
    coreMeta = function(textId, type, name = NULL) {

      card <- identity(cls = class(self)[1], name = name)

      private$meta$created(id = card$id, name = card$name, cls = class(self)[1],
                           description = card$description, type = type,
                           textId = textId)

      invisible(self)
    }
  ),
  public = list(
    initialize = function() {stop("This method is not implemented for this abstract class.")}
  )
)
