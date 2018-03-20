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
      cMetaNames <- names(cMeta)
      dMeta <- as.data.frame(x$docMeta())
      dMetaNames <- colnames(dMeta)

      # Create named corpus vectors, one document per vector
      docs <- x$getDocument()
      content <- unlist(lapply(docs, function(d) {
        paste(d$content, collapse = "")
      }))
      names(content) <- dMeta$id

      # Create tm corpus object
      tmSource <- tm::VectorSource(content)
      tmCorpus <- tm::Corpus(tmSource)

      # Create corpus level meta data
      if (length(cMeta) > 0) {
        for (i in 1:length(cMeta)) {
          NLP::meta(tmCorpus, tag = cMetaNames[i], type = "corpus") <- cMeta[[i]]
        }
      }

      return(tmCorpus)
    },

    from = function(x) {

      # Obtain metadata
      cMeta <- NLP::meta(x, type = "corpus")
      dMeta <- lapply(seq_along(x), function(m) {
        NLP::meta(x[[m]], type = "local")
      })

      # Create corpus object and meta data
      corpus <- Corpus$new()
      keys <- names(cMeta)
      values <- cMeta
      corpus$meta(key = keys, value = values)

      print(length(x))

      # Add documents
      for (i in 1:length(x)) {
        d <- Document$new(x[[i]]$content)
        corpus$addDocument(d)
      }

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

      private$..event <- paste0("Initiated ", private$..classname)
      private$logIt()

      invisible(self)


    },
    #-------------------------------------------------------------------------#
    #                           Conversion Methods                            #
    #-------------------------------------------------------------------------#
    convert = function(x) {

      private$..methodName <- 'convert'

      if (class(x)[1] == "Corpus") {
        return(private$to(x))
      } else if (class(x)[1] %in% c('VCorpus', 'SimpleCorpus', 'PCorpus')) {
        return(private$from(x))
      } else {
        private$..event <- paste0("Invalid class.  The class operates on ",
                                  "'Corpus', 'VCorpus', 'PCorpus', and ",
                                  "'SimpleCorpus' objects only.  See ?",
                                  class(self)[1], " for further assistance.")
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
