#' Search relation in CEDAR
#'
#' Find provisional relation by id. DISCLAIMER: at current development stage of
#' the API, this function is not operational (empty content) !
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param id character. Provisional relation short identifier.
#' @param output.mode character. "full" will return the whole response object
#' (from {httr}) or "content" will fetch the interest values from the response
#' object. Getting the whole object might be interesting to have a look at system
#' metadata, or in case of error to debug the connection. (defaults to "content")
#'
#' @details
#'
#' DISCLAIMER: at current development stage of the API, this function is not
#' operational !
#'
#' This function matches the following queries from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize {
#'   \item {`/relations/{id}`}
#' }
#'
#' @return
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned,
#' containing database metadata and the interesting information in the
#' `collection` subitem.
#'
#' @examples
#' my.api.key <- readline()
#'
#' result <- cedarr::accessRelation(
#'   my.api.key,
#'   id = ???
#' )
#
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessRelation <- function(
  api.key,
  id,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(id))
    stop("No relation ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(id) || is.na(id))
    addError(
      msg = "Invalid value for `id`: must be either a length-one character.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  check <- checkLength(
    c("id"),
    check = check, env = environment()
  )

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0("https://terminology.metadatacenter.org/bioportal/relations/", id),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
