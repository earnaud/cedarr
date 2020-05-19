#' Search properties in CEDAR
#'
#' Function to query properties from ontologies registered in the CEDAR terminology
#' metadata center. (https://terminology.metadatacenter.org/api/#/).
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param query character. Input query as a text.
#' @param sources character. Either value sets collection names or ontologies names in which to get
#' the results of the query.
#' @param exact.match logical Restricts results only to the exact matches of the query
#' in the property id, label, or the generated label (a label, auto-generated from the id).
#' (defaults to FALSE)
#' @param require.definition logical. Filter results only to those that include definitions.
#' (defaults to FALSE).
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#' @param page integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults to 50).
#'
#' @example
#' my.api.key <- readline()
#'
#' result <- cedarr::query(
#'   my.api.key,
#'   "habitat",
#'   "ENVO"
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
propertySearch <- function(
  api.key,
  query,
  sources = NA_character_,
  exact.match = FALSE,
  require.definitions = FALSE,
  output.mode = "content",
  page = 1,
  page.size= 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(query))
    stop("No query provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(query) || query == "")
    addError(
      msg = "Invalid query: must be at least one word length.",
      argcheck = check
    )
  if(!is.character(sources) && !is.na(sources))
    addError(
      msg = "Invalid type for `sources`.",
      argcheck = check
    )
  if(!is.logical(exact.match) ||
      !(isTRUE(exact.match) || isFALSE(exact.match)))
    addError(
      msg = "Invalid value for `exact.match`. Must be TRUE or FALSE.",
      argcheck = check
    )
  if(!is.logical(require.definitions) ||
      !(isTRUE(require.definitions) || isFALSE(require.definitions)))
    addError(
      msg = "Invalid value for `require.definitions`. Must be TRUE or FALSE.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )
  if(!is.numeric(page) || page == 0)
    addError(
      msg = "Invalid value for `page`.",
      argcheck = check
    )
  if(!is.numeric(page.size) || page.size == 0)
    addError(
      msg = "Invalid value for `page.size`.",
      argcheck = check
    )

  # Correction ====
  if(is.na(sources))
    sources <- NULL
  sapply(c("exact.match","require.definitions","output.mode", "page","page.size"), function(arg){
    if(length(get(arg)) > 1){
      assign(arg, get(arg)[1])
      addWarning(
        msg = "`",arg,"` argument had length > 1: only the first element is used.",
        argcheck = check
      )
    }
  })

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/property_search",
    query = list(
      q = query,
      exact_match = exact.match,
      require_definitions = require.definitions,
      suggest = suggest,
      page = page,
      page_size = page.size
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
