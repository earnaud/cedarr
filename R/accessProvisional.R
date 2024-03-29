#' Access Provisional Classes
#'
#' Get all provisional classes in a particular ontology (including provisional
#' value sets and provisional values).
#'
#' @param api_key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display. In this context, ontology
#' can be set to NA: this will list all provisional classes from CEDAR.
#' @param output_mode character. "full" will return the whole response object
#' (from {httr}) or "content" will fetch the interest values from the response
#' object. Getting the whole object might be interesting to have a look at system
#' metadata, or in case of error to debug the connection. (defaults to "content")
#' @param page.index integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults
#' to 50).
#'
#' @details
#'
#' This function matches the following query from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`ontology/{ontology}/classes/provisional`}
#' }
#'
#' @return
#'
#' Since this function targets classes provisionnaly available, it is expected
#' to get an empty result.
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
#' result <- cedarr::accessProvisional(
#'   my_api_key,
#'   "ENVO"
#' )
#'
#' View(result)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkNumber checkString
accessProvisional <- function(
  api_key,
  ontology = NA_character_,
  output_mode = "content",
  page.index = 1,
  page.size = 50
){
  assert(combine = "and",
    # Missing ====
    !anyMissing(api_key),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content")),
    checkNumber(page.index),
    checkNumber(page.size),
    checkString(ontology, na.ok = TRUE)
  )

  # Correction ====
  if(is.na(ontology))
    url <- "https://terminology.metadatacenter.org/bioportal/classes/provisional"
  else # ontology is character
    url <- paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology,
      "/classes/provisional"
    )

  # Request ====
  result <- cedarGet(
    api_key,
    url,
    query = list(
      page = page.index,
      page_size = page.size
    ),
    output_mode = output_mode
  )

  # Output ====
  return(result)
}


