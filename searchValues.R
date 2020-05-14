# Define function ====

#' CEDAR Search Values
#'
#' Search for ontology classes, value sets, and values in the CEDAR API
#' (https://terminology.metadatacenter.org/api/#!/Values/get_search).
#'
#' @param cedar.client a cedar.client for BioPortal.
#' @param query character. Input query as a text.
#' @param scope character. Which search scopes shall be investigated. Accepted values are 1-length vector:
#' "all", "classes", "value_sets", "values".
#' @param as.list logical. Return the value as a R list? (defaults to TRUE)
#' @param ontologies character. IDs of ontologies to query, aka sources. (e.g. ENVO).
#' @param suggest logical. Will perform a search specifically geared towards type-ahead suggestions
#' (defaults to FALSE).
# @param subtree_root_id character. Class identifier that limits the search to the branch rooted on that class.
# It must be URL encoded.
# @param source character. Ontology for which the subtree search will be performed.
# @param maxDepth integer. Subtree depth.
#' @param page integer. See cedar.client documentation. (defaults to `cedar.client@page`).
#' @param pageSize integer. See cedar.client documentation. (defaults to `cedar.client@pageSize`).
#'
#' @importFrom httr POST
cedar.search.values <- function(
  cedar.client,
  query,
  scope = "all",
  as.list = TRUE,
  ontologies = NA_character_,
  suggest = FALSE,
  # max.depth = 1,
  page = 1,
  pageSize = 50
) {
  # Validity check
  if(missing(cedar.client))
    stop("No API client provided: see ?cedar.client.")
  if(missing(query))
    stop("No query provided.")
  if(is.na(cedar.client@api.key))
    stop("Invalid BioOntology API key. You can verifiy it in your account settings.")

  # Request
  .result <- GET(
    "https://terminology.metadatacenter.org/bioportal/search?",
    query = list(
      q = query,
      scope = scope,
      sources = ontologies,
      # maxDepth = max.depth,
      page = page,
      page_size = page.size
    ),
    add_headers(Authorization = paste("apiKey ", cedar.client@api.key)) #"apiKey 59baba4f-cb34-49a6-89fb-76741faa4efd")
  )

  # Output
  return(.result)
}
