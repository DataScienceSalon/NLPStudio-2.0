#==============================================================================#
#                             Validation Functions                             #
#==============================================================================#
#' validateDiscrete
#'
#' \code{validateDiscrete} Validates a parameters that take discrete values.
#'
#' Class validates objects which take discrete parameters. The object must
#' have a public member called params which is a list containing three
#' elements: (1) a vector of variable names, (2) a vector containing
#' the values of the variables, and (3) a list of vectors, where each vector
#' contains the valid values for each of the variables.
#'
#' @usage validateDiscrate(params)
#'
#' @param object The object to be validated
#'
#' @return a list containing two elements: a code and a message. If the
#' validation passed, the code will be TRUE and the msg element will be NULL.
#' Otherwise, the code will be FALSE and the msg element will describe
#' the error.
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateDiscrete <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$discrete$values) > 0) {
    for (i in 1:length(params$discrete$values)) {
      if (!(params$discrete$values[i] %in% params$discete$valid[[i]])) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid '", params$discrete$variables[i], "' parameter. ",
                                  "Valid values are ",
                                  paste0("c(",gsub(",$", "",
                                                   paste0("'",params$discrete$valid[[i]],
                                                          "',", collapse = "")),
                                         "). "),
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
    }
  }
    return(status)
}

#==============================================================================#
#' validateLogical
#'
#' \code{validateLogical} Validates a parameters that take logical values.
#'
#' Class accepts an object with a public parameters member. The parameters
#' member must be a list with two elements: (1) a vector of variable
#' names, and (2) a vector containing the values.
#'
#' @usage validateLogical(object)
#'
#' Class accpe
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateLogical <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$logicals$values) > 0) {
    for (i in 1:length(params$logicals$values)) {
      if (!(params$logicals$values[i] %in% c('TRUE', 'FALSE', 1, 0))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("TRUE/FALSE expected for the '",
                                  params$logicals$variables[i],  "' variable. ",
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
    }
  }
  return(status)
}

#==============================================================================#
#' validatePatternReplace
#'
#' \code{validatePatternReplace} Validates a parameters that contain pattern and replacements
#'
#' Function accepts an object with a public parameters member. The parameters
#' member has one of two formats.
#'
#' Single Element Format: This must be a data frame containing both patterns
#' and their replacements.
#'
#' Two Element Format: The first element is a vector containing the patterns
#' to replace.  The second element must be the replacement vector. This second
#' vector  may be of length one, in which case, every pattern will replaced with this
#' single element. Alternatively the replacement vector must be the same
#' length of the pattern vector.
#'
#' @usage validatePatternReplace(object)
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validatePatternReplace <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  # Validate pattern replace
  replacement <- character()
  pattern <- character()
  if (length(params$pattern) > 0) {
    if (is.data.frame(params$pattern)) {
      if (ncol(params$pattern) == 2) {
        pattern <- as.character(params$pattern[,1])
        replacement <- as.character(params$pattern[,2])
      } else {
        pattern <- as.character(params$pattern[,1])
        replacement <- as.character(params$replacement)
      }
    } else {
      pattern <- as.character(params$pattern)
      replacement <- as.character(params$replacement)
    }
  }

  if (length(replacement) > 0 | length(pattern) > 0) {
    if ((length(replacement) != 1) &
        (length(replacement) != length(pattern))) {
      status[['code']] <- FALSE
      status[['msg']] <- paste0("Replacement values must be of length one",
                                ifelse(length(pattern) == 1,"",
                                       paste0(" or of length ", length(pattern))),
                                ". See ?", class(object)[1],
                                " for further assistance")
    }
  }
  return(status)
}
