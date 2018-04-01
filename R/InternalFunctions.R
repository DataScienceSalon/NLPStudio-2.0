#==============================================================================#
#                             Internal Functions                               #
#==============================================================================#
#                                Proper                                        #
#------------------------------------------------------------------------------#
#' proper
#'
#' \code{proper} Converts text string to proper case
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
proper <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
#------------------------------------------------------------------------------#
#                                 Identity                                     #
#------------------------------------------------------------------------------#
#' identity
#'
#' \code{identity} Creates identity card which are assigned to all business domain objects.
#'
#' @param cls Character string containing the class of the object
#' @param name Character string containing the name of the object. Optional. If
#' NULL, a name is generated from the class and id.
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
identity <- function(cls, name = NULL) {

  card <- list()

  # Creates unique identifier and create object metadata
  settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
  hashid <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                              sample(1:1000, 1, replace = TRUE), settings)
  name <-  ifelse(is.null(name), paste0(cls," (", toupper(hashid),
                                        ")"), name)
  card$id <-  toupper(hashid)
  card$class <- cls
  card$name <- name
  card$description <- paste0(cls, " object '", name, "' created on ",
                        format(Sys.Date(), "%a %b %d %Y"),
                        " by ", Sys.info()[['user']], ".")

  return(card)
}
