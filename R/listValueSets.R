#' List value sets and value sets collections
#'
#' Find all value sets or value sets collections.
#'
#' @param api_key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param output_mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#'
#' @return
#'
#' An exhaustive list of value sets for any value set collection.
#'
#' @examples
#' \dontrun{
#' my_api_key <- readline()
#'
#' result1 <- cedarr::listValueSets(
#'   my_api_key
#' )
#'
#' View(result1)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkString
listValueSets <- function(
  api_key,
  output_mode = "content"
){
  assert(combine = "and",
    # Missing ====
    !anyMissing(api_key),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content"))
  )

  # Request ====
  result <- cedarGet(
    api_key,
    "https://terminology.metadatacenter.org/bioportal/value-sets",
    output_mode = output_mode
  )

  # Output ====
  return(result)
}

#' @describeIn listValueSets
#'
#' @return
#'
#' An exhaustive list of the VS collections in CEDAR.
#'
#' If `output_mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output_mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @examples
#' \dontrun{
#' my_api_key <- readline()
#'
#' result2 <- cedarr::listVSCollections(
#'   my_api_key,
#' )
#'
#' View(result2)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkString
listVSCollections <- function(
  api_key,
  output_mode = "content"
){
  assert(combine = "and",
    # Missing ====
    !anyMissing(api_key),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content"))
  )

  # Request ====
  result <- cedarGet(
    api_key,
    "https://terminology.metadatacenter.org/bioportal/vs-collections",
    output_mode = output_mode
  )

  # Output ====
  return(result)
}
