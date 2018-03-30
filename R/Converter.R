#==============================================================================#
#                                 Converter                                    #
#==============================================================================#
#' Converter
#'
#' \code{Converter} Converts Corpus objects to and from formats of other NLP packages
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{new(...)}}{Instantiates the Converter object.}
#'   \item{\code{quanteda()}}{Converts to and from quanteda format.}
#'   \item{\code{tm()}}{Converts to and from tm format.}
#' }
#'
#'
#' @param x Object to be converted
#' @return Converted object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter family of classes
#' @export
Converter <- R6::R6Class(
  classname = "Converter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() { invisible(self) },
    #-------------------------------------------------------------------------#
    #                           Converter Methods                             #
    #-------------------------------------------------------------------------#
    quanteda = function(x) {
      return(ConverterQuanteda$new()$convert(x))
    },

    tm = function(x) {
      return(ConverterTM$new()$convert(x))
    }
  )
)
