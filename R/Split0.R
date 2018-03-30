#==============================================================================#
#                                   Split0                                     #
#==============================================================================#
#' Split0
#'
#' \code{Split0} Abstract class that defines the methods for the Corpus and Dcocument Split classes
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Split Classews
#' @export
Split0 <- R6::R6Class(
  classname = "Split0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..cvSets = list(),

    initCvSets = function() {

      # Designate names
      if (is.null(private$..setNames)) {
        private$..setNames <- paste0("CV-Set-", c(1:length(private$..splits)))
      }

      # Obtain metadata, except id, name, and class
      meta <- private$..x$meta()
      keys <- names(meta$object)
      values <- meta$object
      values <- values[!keys %in% c("id", "class")]
      keys <- keys[!keys %in% c("id", "class")]

      # Create cross-validation sets and assign metadata from master corpus
      private$..cvSets <- lapply(seq_along(c(1:length(private$..splits))), function(x) {
        cvSet <- Corpus$new()
        cvSet <- cvSet$meta(key = keys, value = values)
        cvSet <- cvSet$meta(key = "name",
                            value = paste0(private$..x$meta(key = "name"),
                                           " (", private$..setNames[x], ")"))
        cvSet <- cvSet$meta(key = "cv", value = private$..setNames[x])
        cvSet
      })

      names(private$..cvSets) <- private$..setNames

      return(TRUE)
    }
  ),

  public = list(

    initialize = function(x, ...) { stop("This method is not implemented for this abstract class") },
    execute = function() { stop("This method is not implemented for this abstract class") }
  )
)
