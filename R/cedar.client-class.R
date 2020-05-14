# Set cedar.client ====

#' CEDAR API Access client turned into an R class.
#'
#' @slot apikey  An API Key is required to access any API call. It is used within BORAA methods as
#' a header for http requests. An API key is linked to a CEDAR account (\url{cedar.metadatacenter.org}).
#'
#' @export
cedar.client <- setClass(
  Class = "cedar.client",
  slots = c(
    api.key = "character"
  ),
  prototype = prototype(
    api.key = NA_character_
  ),
  validity = function(object){
    errors <- character()

    # api.key
    if(length(object@api.key) < 1)
      errors <- c(errors, "No API key provided: without this item, no authentication is
      possible. Check your account settings at http://wwww.bioontology.org.")
    else if(length(object@api.key) > 1)
      errors <- c(errors, "More than one API key provided.")

    if(length(errors) == 0) TRUE else errors
  }
)
