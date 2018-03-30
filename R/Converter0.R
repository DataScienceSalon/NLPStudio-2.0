#==============================================================================#
#                                   Converter0                                 #
#==============================================================================#
#' Converter0
#'
#' \code{Converter0} Abstract class for Converter classes.
#'
#' Defines interface for concrete Converter classes.
#'
#' @template converterMethods
#'
#' @param x Object to be converted
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter family of classes
#' @export
Converter0 <- R6::R6Class(
  classname = "Converter0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this abstract class.") },
    #-------------------------------------------------------------------------#
    #                           Converter Methods                             #
    #-------------------------------------------------------------------------#
    convert = function() { stop("This method is not implemented for this abstract class.") }
  )
)
