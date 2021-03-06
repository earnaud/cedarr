#' Search properties in CEDAR
#'
#' Function to query properties from ontologies registered in the CEDAR terminology
#' metadata center. (https://terminology.metadatacenter.org/api/#/).
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param query character. Input query as a text.
#' @param sources character. Either value sets collection names or ontologies
#' names in which to get the results of the query.
#' @param exact.match logical Restricts results only to the exact matches of the
#' query in the property id, label, or the generated label (a label, auto-generated
#'  from the id). (defaults to FALSE)
#' @param require.definitions logical. Filter results only to those that include
#' definitions. (defaults to FALSE).
#' @param output.mode character. "full" will return the whole response object
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
#'   \item{`/property_search`}
#' }
#'
#' @return
#'
#' A search result with the properties corresponding to the query.
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @examples
#' my.api.key <- readline()
#'
#' # Search for a property matching "has curation status"
#'
#' result <- cedarr::propertySearch(
#'   my.api.key,
#'   "has curation status",
#'   "ENVO"
#' )
#'
#' View(result)
#'
#' @export
#' @importFrom ArgumentCheck newArgCheck addError finishArgCheck
propertySearch <- function(
  api.key,
  query,
  sources = NA_character_,
  exact.match = FALSE,
  require.definitions = FALSE,
  output.mode = "content",
  page.index = 1,
  page.size= 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API key provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(query))
    stop("No query provided.")

  # Invalid ====
  check <- ArgumentCheck::newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode", "page.index", "page.size"),
    check = check, env = environment()
  )

  if(!is.character(query) || query == "")
    ArgumentCheck::addError(
      msg = "Invalid query: must be at least one word length.",
      argcheck = check
    )
  if(!is.character(sources) && !is.na(sources))
    ArgumentCheck::addError(
      msg = "Invalid type for `sources`.",
      argcheck = check
    )
  if(!is.logical(exact.match) ||
      !(isTRUE(exact.match) || isFALSE(exact.match)))
    ArgumentCheck::addError(
      msg = "Invalid value for `exact.match`. Must be TRUE or FALSE.",
      argcheck = check
    )
  if(!is.logical(require.definitions) ||
      !(isTRUE(require.definitions) || isFALSE(require.definitions)))
    ArgumentCheck::addError(
      msg = "Invalid value for `require.definitions`. Must be TRUE or FALSE.",
      argcheck = check
    )

  # Correction ====
  if(is.na(sources))
    sources <- NULL

  check <- checkLength(
    c("api.key", "exact.match","require.definitions","output.mode", "page.index",
      "page.size"),
    check = check, env = environment()
  )

  ArgumentCheck::finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/property_search",
    query = list(
      q = query,
      exact_match = exact.match,
      require_definitions = require.definitions,
      page = page.index,
      page_size = page.size
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
