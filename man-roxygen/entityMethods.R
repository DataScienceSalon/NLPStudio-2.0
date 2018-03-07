#' @section IO Methods:
#'  \itemize{
#'   \item{\code{read(path, io = NULL)}}{Reads an object from a specified
#'   file using the default or designated IO Strategy. See IOStrategy?
#'   for details w.r.t. the io parameter. If the IO strategy is not
#'   indicated, the default io strategy for the file type is used.}
#'   \item{\code{write(path, io = NULL)}}{Writes an object to a specified
#'   file using the default or designated IO Strategy. See IOStrategy?
#'   for details w.r.t. the io parameter. If the IO strategy is not
#'   indicated, the default io strategy for the file type is used.}
#'  }
#'
#' @section Metadata Method:
#'  \itemize{
#'   \item{\code{meta(key = NULL, value = NULL)}}{Provides facility for
#'   managing an object's metadata represented as key/value pairs in
#'   a list format. Metadata may be indicated via single key and
#'   value character strings or by pairs of character vectors. If
#'   no parameters are passed to the method, then it returns the
#'   current metadata, as well as system generated application
#'   and system metadata, in data frame format. If a character
#'   string or vector is passed via the key parameter, the value
#'   or values associated with the keys are returned.}
#' }
