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
    ..codes = list(
      null = 0,
      sub = 26
    ),
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

    appendCode = function(code) {
      private$..codes[[names(code)]] <- code
      invisible(self)
    },

    repair = function() {

      content <- private$..io$bin$read(path = private$..path)

      for (i in 1:length(private$..codes)) {
        content[content == as.raw(private$..codes[[i]])] = as.raw(32)
      }

      # Save to temp file and re-read
      d <- tempfile(fileext = '.txt')
      private$..io$bin$write(path = d, content = content)
      content <- private$..io$text$read(path = d)

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
      if (!R.utils::isFile(path)) stop(paste("File", path, "does not exist."))

      # Initiate variable and IO methods
      private$..path <- path
      private$..io$bin <- IOBin$new()
      private$..io$text <- IOText$new()

      # Create log entry
      private$..event <- paste0("Instantiated RepairFile for ", path, ".")
      private$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         ASCII Code Methods                              #
    #-------------------------------------------------------------------------#
    null = function() { return(private$appendCode(private$..ctrl$null)) },
    soh = function() { return(private$appendCode(private$..ctrl$soh)) },
    stx = function() { return(private$appendCode(private$..ctrl$stx)) },
    etx = function() { return(private$appendCode(private$..ctrl$etx)) },
    eot = function() { return(private$appendCode(private$..ctrl$eot)) },
    enq = function() { return(private$appendCode(private$..ctrl$enq)) },
    ack = function() { return(private$appendCode(private$..ctrl$ack)) },
    bel = function() { return(private$appendCode(private$..ctrl$bel)) },
    bs = function() { return(private$appendCode(private$..ctrl$bs)) },
    ht = function() { return(private$appendCode(private$..ctrl$ht)) },
    lf = function() { return(private$appendCode(private$..ctrl$lf)) },
    vt = function() { return(private$appendCode(private$..ctrl$vt)) },
    ff = function() { return(private$appendCode(private$..ctrl$ff)) },
    cr = function() { return(private$appendCode(private$..ctrl$cr)) },
    so = function() { return(private$appendCode(private$..ctrl$so)) },
    si = function() { return(private$appendCode(private$..ctrl$si)) },
    dle = function() { return(private$appendCode(private$..ctrl$dle)) },
    dc1 = function() { return(private$appendCode(private$..ctrl$dc1)) },
    dc2 = function() { return(private$appendCode(private$..ctrl$dc2)) },
    dc3 = function() { return(private$appendCode(private$..ctrl$dc3)) },
    dc4 = function() { return(private$appendCode(private$..ctrl$dc4)) },
    nak = function() { return(private$appendCode(private$..ctrl$nak)) },
    syn = function() { return(private$appendCode(private$..ctrl$syn)) },
    etb = function() { return(private$appendCode(private$..ctrl$etb)) },
    can = function() { return(private$appendCode(private$..ctrl$can)) },
    em = function() { return(private$appendCode(private$..ctrl$em)) },
    sub = function() { return(private$appendCode(private$..ctrl$sub)) },
    esc = function() { return(private$appendCode(private$..ctrl$esc)) },
    fs = function() { return(private$appendCode(private$..ctrl$fs)) },
    gs = function() { return(private$appendCode(private$..ctrl$gs)) },
    rs = function() { return(private$appendCode(private$..ctrl$rs)) },
    us = function() { return(private$appendCode(private$..ctrl$us)) },

    #-------------------------------------------------------------------------#
    #                            Execute Method                               #
    #-------------------------------------------------------------------------#
    execute = function() {
      content <- private$repair()
      return(content)
    },

    #-------------------------------------------------------------------------#
    #                            Visitor Method                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$repairFile(self)
    }
  )
)
