#' TermFreqQ
#'
#' \code{TermFreqQ} Term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqStrategyQ}} class and methods for manipulating it.
#'
#' TermFreqQ objects are wrappers for the \code{\link[quanteda]{dfm}} and
#' related classes.
#'
#' @usage dfm <- TermFreqQ$new(x = dfm)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqQ class.}
#'   \item{\code{group(groups = NULL, fill = FALSE)}}{Combine documents
#'    in a dfm by a grouping variable. See \code{\link[quanteda]{dfm_group}}
#'    for further assistance. }
#'   \item{\code{lookup(dictionary, levels = 1:5, exclusive = TRUE,
#'   valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
#'   capkeys = !exclusive, nomatch = NULL,
#'   verbose = quanteda_options("verbose"))}}{Apply a dictionary to a
#'   dfm by looking up all dfm features for matches in a a set of dictionary
#'   values, and replace those features with a count of the dictionary’s keys.
#'   See \code{\link[quanteda]{dfm_lookup}} for
#'   further assistance.}
#'   \item{\code{replace(pattern, replacement = NULL, case_insensitive = TRUE,
#'   verbose = quanteda_options("verbose"))}}{Substitute features based on
#'   vectorized one-to-one matching for lemmatization or user-defined stemming.
#'   See \code{\link[quanteda]{dfm_replace}} for further assistance.}
#'   \item{\code{sample(size = ndoc(x), replace = FALSE, prob = NULL,
#'   margin = c("documents", "features"))}}{Sample randomly from a dfm
#'   object, from documents or features. See \code{\link[quanteda]{dfm_sample}}
#'   for further assistance.}
#'   \item{\code{select(pattern = NULL, selection = c("keep", "remove"),
#'   valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
#'   min_nchar = 1L, max_nchar = 79L, verbose = quanteda_options("verbose"))}}
#'   {This method selects or removes features from a dfm or fcm, based on
#'   feature name matches with pattern.
#'   See \code{\link[quanteda]{dfm_select}} for further assistance.}
#'   \item{\code{sort(decreasing = TRUE, margin = c("features", "documents", "both")))}}
#'   {Sorts a dfm by descending frequency of total features, total features in
#'   documents, or both.}
#'   \item{\code{subset(subset, select)}}{Returns document subsets of a dfm
#'   that meet certain conditions, including direct logical operations on
#'   docvars (document-level variables).}
#'   \item{\code{tfidf(scheme_tf = "count", scheme_df = "inverse", base = 10)}}
#'   {Weight a dfm by term frequency-inverse document frequency (tf-idf),
#'   with full control over options. Uses fully sparse methods for efficiency.}
#'   \item{\code{tolower(keep_acronyms = FALSE)}}{Convert the features of the dfm to
#'   lower case and then recombine the counts.}
#'   \item{\code{toupper(keep_acronyms = FALSE)}}{Convert the features of the dfm to
#'   upper case and then recombine the counts.}
#'   \item{\code{trim(min_count = 1, min_docfreq = 1, max_count = NULL,
#'   max_docfreq = NULL, sparsity = NULL, verbose = quanteda_options("verbose"))}}
#'   {Returns a document by feature matrix reduced in size
#'   based on document and term frequency, usually in terms of a minimum
#'   frequencies, but may also be in terms of maximum frequencies. Setting a
#'   combination of minimum and maximum frequencies will select features
#'   based on a range.}
#'   \item{\code{weight(x, scheme = c("count", "prop", "propmax", "logcount",
#'   "boolean", "augmented", "logave"), weights = NULL, base = 10, K = 0.5))}}
#'   {Weight the feature frequencies in a dfm.}
#'   \item{\code{group()}}{Combine documents in a dfm by a grouping variable.}
#'
#'
#'
#'  }
#' @template entityMethods
#'
#' @param x A quanteda dfm object.
#' @param pattern a character vector or dictionary.
#' @param replacement if pattern is a character vector, then
#' replacement must be character vector of equal length, for a 1:1 match.
#' @param groups a parameter for the group method. Either: a character
#' vector containing the names of document variables to be used for
#' grouping; or a factor or object that can be  coerced into a factor
#' equal in length or rows to the number of documents.
#' See \code{\link[quanteda]{dfm_group}} for further assistance.
#' @param fill logical. A parameter for the group method. If TRUE and
#' groups is a factor, then use all levels of the factor when
#' forming the new "documents" of the grouped dfm
#' See \code{\link[quanteda]{dfm_group}} for further assistance.
#' @param dictionary a quanteda dictionary class object. See
#' \code{\link[quanteda]{dictionary}} for further reference.
#' @param levels levels of entries in a hierarchical dictionary that
#' will be applied.
#' @param exclusive if TRUE, remove all features not in dictionary,
#' otherwise, replace values in dictionary with keys while leaving other
#' features unaffected.
#' @param valuetype the type of pattern matching: "glob" for
#' "glob"-style wildcard expressions; "regex" for regular expressions;
#' or "fixed" for exact matching.
#' @param case_insensitive ignore the case of dictionary values if TRUE
#' @param capkeys if TRUE, convert dictionary keys to uppercase
#' to distinguish them from other features.
#' @param nomatch an optional character naming a new feature
#' that will contain the counts of features of x not matched to a
#' dictionary key. If NULL (default), do not tabulate unmatched features.
#' @param verbose print status messages if TRUE
#' @param size a positive number, the number of documents or features to select
#' in the sample method.
#' @param replace logical; should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of
#' the vector being sampled.
#' @param margin dimension (of a dfm) to sample: can be documents or features
#' @param selection whether to keep or remove the features. Values are
#' c("keep", "remove").
#' @param min_nchar,max_nchar numerics specifying the minimum and maximum
#' length in characters for features to be removed or kept; defaults are
#' 1 and 79. (Set max_nchar to NULL for no upper limit.) These are
#' applied after (and hence, in addition to) any selection based on pattern
#' matches.
#' @param decreasing logical; if TRUE, the sort (sort method) will be
#' in descending order, otherwise sort in increasing order.
#' @param margin which margin to sort on features to sort by frequency of
#' features, documents to sort by total feature counts in documents, and both
#' to sort by both.
#' @param select expression, indicating the docvars to select from the dfm;
#' or a dfm object, in which case the returned dfm will contain the same
#' documents as the original dfm, even if these are empty.
#' @param scheme Parameter for the weight method. See \code{\link[quanteda]{dfm_weight}}
#' for further reference.
#' @param scheme_tf scheme for dfm_weight; defaults to "count"
#' @param scheme_df scheme for docfreq; defaults to "inverse".
#' @param base the base for the logarithms in the tf and docfreq calls; default is 10
#' @param keep_acronyms logical; if TRUE, do not lowercase any all-uppercase words
#' (applies only to tolower functions)
#' @param min_count,max_count minimum/maximum count or percentile frequency of
#' features across all documents, below/above which features will be removed.
#' @param min_docfreq,max_docfreq minimum/maximum number or fraction of
#' documents in which a feature appears, below/above which features will
#' be removed sparsity equivalent to 1 - min_docfreq.
#' @param weights Parameter used in the weight method. \code{\link[quanteda]{dfm_weight}}
#' for further reference.
#' @param K the K for the augmentation when scheme = "augmented" in the weight
#' method.
#' @param smoothing constant added to the dfm cells for smoothing, default is 1
#'
#' @template entityParams
#' @param x The Corpus object which was tokenized.
#' @param what Character string indicating the level of tokenization. Valid
#' values are c("sentence", "word", "character").
#' @template metaParams
#'
#' @return TermFreqQ object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @family Tokens Classes
#' @export
TermFreqQ <- R6::R6Class(
  classname = "TermFreqQ",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataCollection0,

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL, corpusId = corpusId, what = c('word')) {

      private$loadDependencies(name = name)

      # Validation
      private$..params <- list()
      private$..params$discrete$variables <- c('what')
      private$..params$discrete$values <- c(what)
      private$..params$discrete$valid <- list(c('sentence', 'word','character'))
      if (private$validate()$code == FALSE) stop()

      private$..what <- what
      private$coreMeta(name = name,
                       type = paste0(proper(what), " Tokens"),
                       corpusId = corpusId)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                        Document Metadata Method                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, values = NULL) {

      if (is.null(key)) {
        dm <- rbindlist(lapply(private$..attachments[['TokensDocument']], function(a) {
            a$meta()$object
          }))
        return(dm)
      } else if (length(key) != 1) {
        event <- paste0("Key parameter must be of length one. ",
                        "See ?", class(self)[1], " for further ",
                        "assistance.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      } else  if (length(values) != 1) {
        if (length(values) != length(private$..attachments[['TokensDocument']])) {
          event <- paste0("Unable to add metadata. The values ",
                          "parameter must be of length one or ",
                          "length equal to that number of documents ",
                          "in the TermFreqQ object. ",
                          "See ?", class(self)[1], " for further ",
                          "assistance.")
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        }
      } else {
        values <- rep(values, length(private$..attachments[['TokensDocument']]))
      }


      for (i in 1:length(private$..attachments[['TokensDocument']])) {
        private$..attachments[['TokensDocument']][[i]] <-
          private$..attachments[['TokensDocument']][[i]]$metadata(key = key, value = values[i])
      }

      event <- paste0("Updated TokensDocument' metadata.")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, stats = TRUE, system = TRUE, state = TRUE,
                       quiet = FALSE, abbreviated = FALSE, attachments = TRUE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta, quiet = quiet)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta = meta, quiet = quiet)
          section <- c("Core Metadata")
        }

        if (attachments) {
          result$attachments <- private$attachments(quiet = quiet)
          section <- c(section, "Attachments")
        }

        if (state) {
          result$app <- private$state(meta = meta, quiet = quiet)
          section <- c(section, "State Information")
        }

        if (system) {
          result$sys <- private$system(meta = meta, quiet = quiet)
          section <- c(section, "System Information")
        }

        names(result) <- section
      }
      invisible(result)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokensCollection(self)
    }
  )
)
