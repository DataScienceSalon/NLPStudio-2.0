#==============================================================================#
#                               Base                                           #
#==============================================================================#
#' Base
#'
#' \code{Base} Base class from which all classes are descended.
#'
#' This base class defines members and methods common across all classes, such
#' as validation and logging.
#'
#' @section Base methods:
#'  \itemize{
#'   \item{\code{getName()}}{Returns name assigned to an object.}
#'   \item{\code{getParams()}}{Returns the parameters passed to the constructor.}
#'   \item{\code{logIt(level = c("Info", "Warn", "Error"))}}{Writes to the system log.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Base <- R6::R6Class(
  classname = "Base",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..params = list(),
    ..className = character(),
    ..methodName = character(),
    ..event = character(),
    ..logs = character(),
    ..log = data.frame(),

    #-------------------------------------------------------------------------#
    #                           Validate Parameters                           #
    #-------------------------------------------------------------------------#
    # validateParams
    #
    # Initiates parameter validation for common routines. This method takes
    # as its parameter, the calling method name, and initiates the procedure
    # by instantiating a validator object and calling the visitor method
    # associated with the method name parameter. The validator dispatches
    # the appropriate visitor, which performs the validation.
    #
    # Parameter: what The method name requesting validation services.
    #
    # Returns: List containing a logical and a character vector.  If the
    # validation passed, the logical is TRUE and the character vector is
    # null. If not, the logical is FALSE, and the character vector contains
    # text describing the error.
    validateParams = function(what = "initialize") {
      # Valid values are c("init", "associate")

      if (what == "initialize") {
        v <- Validator$new()
        status <- v$init(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      } else if (what == "get") {
        v <- Validator$new()
        status <- v$get(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      } else if (what == "attach") {
        v <- Validator$new()
        status <- v$attach(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      }  else if (what == "detach") {
        v <- Validator$new()
        status <- v$detach(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      } else if (what == "read") {
        v <- Validator$new()
        status <- v$read(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      } else if (what == "source") {
        v <- Validator$new()
        status <- v$source(self)
        if (status$code == FALSE) {
          private$..event <- status$msg
          private$logIt("Error")
        }
      }
      return(status)
    },

    #-------------------------------------------------------------------------#
    #                           Log Methods                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..event
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()

      if (level %in% c("Info", 'info', 'INFO')) {
        log <- data.frame(user = Sys.info()[['user']],
                          date = Sys.time(),
                          event = private$..event)
        private$..log <- rbind(private$..log, log)

      }
    }
  ),
  public = list(

    #-------------------------------------------------------------------------#
    #                           Basic Get  Methods                            #
    #-------------------------------------------------------------------------#
    getId = function() private$..meta$object$id,
    getName = function() private$..meta$object$name,
    getParams = function() private$..params

  )
)
