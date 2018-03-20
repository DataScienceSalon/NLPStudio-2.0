#==============================================================================#
#                                 ConverterTM                                  #
#==============================================================================#
#' ConverterTM
#'
#' \code{ConverterTM} Converts NLPStudio Corpus objects to and from tm corpus objects.
#'
#' @usage ConverterTM$new()$convert(x)
#'
#' @param x Object to be converted
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter Classes
#' @export
ConverterTM <- R6::R6Class(
  classname = "ConverterTM",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Converter0,

  private = list(

    to = function(x) {

      # Extract metadata
      cMeta <- as.list(x$meta()$object)
      dMeta <- as.list(x$docMeta())

      # Create named corpus vectors, one document per vector element
      docs <- x$getDocument()
      content <- unlist(lapply(docs, function(d) {
        paste(d$content, collapse = "")
      }))
      names(content) <- dMeta$name

      # Create tm corpus object
      tmCorpus <- tm::VectorSource(x = content)

      # Create corpus level metadata
      varNames <- names(cMeta)
      for (i in 1:length(varNames)) {
        meta(tmCorpus, tag = varNames[i], type = "corpus") <- cMeta[[i]]
      }

      return(tmCorpus)
    },

    from = function(x) {

      # Obtain metadata
      cMeta <- quanteda::metacorpus(x)
      dMeta <- as.data.frame(quanteda::docvars(x))

      # Create corpus object and meta data
      corpus <- Corpus$new()
      keys <- names(cMeta)
      values <- cMeta
      corpus <- corpus$meta(key = keys, value = values)

      # Add documents
      lapply(seq_along(x$documents$texts), function(t) {
        d <- Document$new(name = as.character(x$documents$name[t]),
                          x = as.character(x$documents$texts[t]))
        corpus$addDocument(d)
      })

      # Add document metadata
      keys <- names(dMeta)
      for (i in 1:ncol(dMeta)) {
        corpus$docMeta(key = keys[i], value = dMeta[,i])
      }
      return(corpus)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$..className <- "ConverterTM"
      private$..methodName <- "initialize"
      private$..logs <- LogR$new()

      private$..state <- paste0("Initiated ", private$..classname)
      self$logIt()

      invisible(self)


    },
    #-------------------------------------------------------------------------#
    #                           Conversion Methods                            #
    #-------------------------------------------------------------------------#
    convert = function(x) {

      private$..methodName <- 'convert'

      if (class(x)[1] == "Corpus") {
        return(private$..to(x))
      } else if (class(x)[1] == "corpus") {
        return(private$..from(x))
      } else {
        private$..event <- paste0("This class operates on Corpus and quanteda ",
                                  "corpus objects only.")
        private$logIt("Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                              Visitor Method                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$converterTM(self)
    }
  )
)
