#' Search values in CEDAR
#'
#' Search for terms, classes or retrieve full ontologies
#' from the CEDAR terminology metadata center.
#' (https://terminology.metadatacenter.org/api/#/).
#'
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param query character. Input query as a text.
#' @param sources character. Either value sets collection names
#' or ontologies names in which to get the results of the query.
#' @param scope character. Which search scopes shall be
#' investigated. Accepted values are 1-length vector: "all"
#' (default), "classes", "value_sets", "values".
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#' @param suggest logical. Will perform a search specifically
#' geared towards type-ahead suggestions (defaults to FALSE).
#' @param subtree.root.id character. URL for the class identifier
#' that limits the search to the branch rooted on that class.
#' @param subtree.source character. URL for the ontology for which the
#' subtree search will be performed. Not evaluated if
#' `subtree.root.id` is not provided.
#' @param maxDepth integer. Subtree depth.Not evaluated if
#' `subtree.root.id` is not provided.
#' @param page.index integer. Index of the page to be returned
#' (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped
#' at 50. (defaults to 50).
#'
#' @details
#'
#' This function matches the following query from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`/search`, for both classes and values}
#' }
#'
#' @return
#'
#' A list or data.frame with detailed information on the queried information and
#' how to access it.
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @examples
#' \dontrun{
#' my.api.key <- readline()
#'
#' result <- cedarr::search(
#'   my.api.key,
#'   "habitat",
#'   "ENVO"
#' )
#'
#' View(result)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkNumber checkString checkLogical
cedarSearch <- function(
  api.key,
  query,
  sources = NA_character_,
  scope = "all",
  suggest = FALSE,
  output.mode = "content",
  subtree.root.id = NA_character_,
  subtree.source = NA_character_,
  maxDepth = 1,
  page.index = 1,
  page.size= 50
){
  # Invalid ====
  assert(combine = "and",
    # Missing
    !anyMissing(c(api.key, query)),
    # Invalid
    checkString(api.key, pattern = "^apiKey"),
    checkCharacter(query, min.chars = 1),
    checkString(sources, na.ok = FALSE),
    checkLogical(suggest),
    checkChoice(scope, c("all", "classes", "value_sets", "values")),
    checkChoice(output.mode, c("full", "content")),
    checkNumber(page.index),
    checkNumber(page.size),
    checkString(subtree.root.id, na.ok = TRUE),
    checkString(subtree.source, na.ok = TRUE),
    checkNumber(maxDepth, lower = 0)
  )

  # Correction ====
  if(is.na(sources))
    sources <- NULL

  if(is.na(subtree.root.id)){
    subtree.root.id <- NULL
    subtree.source <- NULL
  }
  else {
    subtree.root.id <- URLencode(subtree.root.id, reserved = TRUE)
  }

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/search",
    query = list(
      q = query,
      scope = scope,
      sources = sources,
      suggest = suggest,
      subtree_root_id = subtree.root.id,
      source = subtree.source,
      maxDepth = as.integer(maxDepth),
      page = as.integer(page.index),
      page_size = as.integer(page.size)
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}


