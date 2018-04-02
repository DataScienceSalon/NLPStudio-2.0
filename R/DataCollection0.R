#==============================================================================#
#                               DataCollection0                                #
#==============================================================================#
#' DataCollection0
#'
#' \code{DataCollection0} Abstract class for all DataCollection classes
#'
#' This base class defines members and methods common across all DataCollection
#' classes, such as TokensCollection, DFMCollection, POSCollection and
#' StemCollection classes.
#'
#' @section DataCollection0 methods:
#'  \itemize{
#'   \item{\code{attach(x)}}{Method for attaching objects.}
#'   \item{\code{detach(key, value)}}{Method for detaching objects.}
#'   \item{\code{attachments(key = NULL, value = NULL)}}{Lists an object's attachments.}
#'  }
#'
#'  @param x Object or list of objects to be attached
#'  @param key Character string or vector of strings indicating the metadata
#'  variable or variables used for matching
#'  @param value Character string or vector of strings indicating the
#'  metadata value associated with the key(s) parameter.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Collection Classes
#' @export
DataCollection0 <- R6::R6Class(
  classname = "DataCollection0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  private = list(

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    core = function(meta, quiet = FALSE) {

      df <- as.data.frame(meta$core, stringsAsFactors = FALSE,
                          row.names = NULL)

      if (quiet == FALSE)  {
        cat(paste0("\n\n   Object Id : ", meta$core$id))
        cat(paste0("\n   Corpus Id : ", meta$core$corpusId))
        cat(paste0("\nObject Class : ", meta$core$class))
        cat(paste0("\n Object Name : ", meta$core$name))
        cat(paste0("\n Object Type : ", meta$core$type))
        cat(paste0("\n Description : ", meta$core$description))

        otherMeta <- df %>% select(-id, -corpusId, -class, -name, -type,
                                   -description)
        if (ncol(otherMeta) > 0) {
          cat("\n\nAdditional Core Metadata:\n")
          print(otherMeta, row.names = FALSE)
        }
      }
      return(df)
    }
  ),

  public = list(

    initialize = function() {stop("This method is not implemented for this abstract class.")}

  )
)
