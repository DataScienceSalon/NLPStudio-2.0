#==============================================================================#
#                               RepairFile                                     #
#==============================================================================#
#' RepairFile
#'
#' \code{RepairFile} Repairs ASCII encoding in text files
#'
#' Class responsible for performing repairs on text files at binary level. Control
#' characters are converted to ASCII spaces either individually, by calling the
#' method associated with a specific control character, or all control
#' characters can be converted to an ASCII space at once by calling the
#' 'ctrl' method.
#'
#' @param path Character string indicating the path to a file to be repaired.
#' @return content Character vector with repaired content.
#'
#' @examples
#' txtFile <- "./foo.txt"
#' txtContent <- RepairFile$new(txtFile)$null()  # Converts NULL characters to spaces
#' txtContent <- RepairFile$new(txtFile)$sub()  # Converts SUB characters to spaces
#' txtContent <- RepairFile$new(txtFile)$ctrl()  # Converts all control characters to spaces
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextStudio Classes
#' @export
RepairFile <- R6::R6Class(
  classname = "RepairFile",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..path = logical(),
    ..io = list(),
    ..ctrl = list(
      null = 0,
      soh = 1,
      stx = 2,
      etx = 3,
      eot = 4,
      enq = 5,
      ack = 6,
      bel = 7,
      bs = 8,
      ht = 9,
      lf = 10,
      vt = 11,
      ff = 12,
      cr = 13,
      so = 14,
      si = 15,
      dle = 16,
      dc1 = 17,
      dc2 = 18,
      dc3 = 19,
      dc4 = 20,
      nak = 21,
      syn = 22,
      etb = 23,
      can = 24,
      em = 25,
      sub = 26,
      esc = 27,
      fs = 28,
      gs = 29,
      rs = 30,
      us = 31
    ),

    repair = function(ctrl) {

      content <- private$..io$bin$read(path = path)

      for (i in 1:length(ctrl)) {
        content[content == as.raw(ctrl[i])] = as.raw(32)
      }

      # Save to temp file and re-read
      d <- tempfile(fileext = '.txt')
      private$..io$bin$write(path = d, content = content)
      content <- private$..io$txt$read(path = d)

      return(content)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(path) {

      private$..className <- 'RepairFile'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      # Validate path
      if (!R.utils::isFile(path)) stop("File does not exist at path.")

      # Initiate variable and IO methods
      private$..path <- path
      private$..io$bin <- IOBin$new()
      private$..io$text <- IOText$new()

      # Create log entry
      private$..state <- paste0("Instantiated RepairFile for ", path, ".")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Repair Methods                                #
    #-------------------------------------------------------------------------#
    ctrl = function() { private$repair(private$..ctrl) },
    null = function() { private$repair(private$..ctrl$null) },
    soh = function() { private$repair(private$..ctrl$soh) },
    stx = function() { private$repair(private$..ctrl$stx) },
    etx = function() { private$repair(private$..ctrl$etx) },
    eot = function() { private$repair(private$..ctrl$eot) },
    enq = function() { private$repair(private$..ctrl$enq) },
    ack = function() { private$repair(private$..ctrl$ack) },
    bel = function() { private$repair(private$..ctrl$bel) },
    bs = function() { private$repair(private$..ctrl$bs) },
    ht = function() { private$repair(private$..ctrl$ht) },
    lf = function() { private$repair(private$..ctrl$lf) },
    vt = function() { private$repair(private$..ctrl$vt) },
    ff = function() { private$repair(private$..ctrl$ff) },
    cr = function() { private$repair(private$..ctrl$cr) },
    so = function() { private$repair(private$..ctrl$so) },
    si = function() { private$repair(private$..ctrl$si) },
    dle = function() { private$repair(private$..ctrl$dle) },
    dc1 = function() { private$repair(private$..ctrl$dc1) },
    dc2 = function() { private$repair(private$..ctrl$dc2) },
    dc3 = function() { private$repair(private$..ctrl$dc3) },
    dc4 = function() { private$repair(private$..ctrl$dc4) },
    nak = function() { private$repair(private$..ctrl$nak) },
    syn = function() { private$repair(private$..ctrl$syn) },
    etb = function() { private$repair(private$..ctrl$etb) },
    can = function() { private$repair(private$..ctrl$can) },
    em = function() { private$repair(private$..ctrl$em) },
    sub = function() { private$repair(private$..ctrl$sub) },
    esc = function() { private$repair(private$..ctrl$esc) },
    fs = function() { private$repair(private$..ctrl$fs) },
    gs = function() { private$repair(private$..ctrl$gs) },
    rs = function() { private$repair(private$..ctrl$rs) },
    us = function() { private$repair(private$..ctrl$us) },

    #-------------------------------------------------------------------------#
    #                            Visitor Method                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$repairFile(self)
    }
  )
)
