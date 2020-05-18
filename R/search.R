#' Search values in CEDAR
#'
#' Function to query terms, classes or retrieve full ontologies from the CEDAR terminology
#' metadata center. (https://terminology.metadatacenter.org/api/#/).
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param query character. Input query as a text.
#' @param sources character. Either value sets collection names or ontologies names in which to get
#' the results of the query.
#' @param scope character. Which search scopes shall be investigated. Accepted values are 1-length vector:
#' "all" (default), "classes", "value_sets", "values".
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#' @param suggest logical. Will perform a search specifically geared towards type-ahead suggestions
#' (defaults to FALSE).
#TODO @param source character. Ontology for which the subtree search will be performed. It
# must be URL encoded.
#TODO @param subtree_root_id character. Class identifier that limits the search to the branch rooted on that class.
# It must be URL encoded.
#TODO @param maxDepth integer. Subtree depth.
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
query <- function(
  api.key,
  query,
  sources = NA_character_,
  scope = "all",
  suggest = FALSE,
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
  if(!is.character(sources) || !is.na(sources))
    addError(
      msg = "Invalid type for `sources`.",
      argcheck = check
    )
  if(!is.character(scope) ||
      length(scope) == 0 ||
      !scope %in% c("all", "classes", "value_sets", "values"))
    addError(
      msg = "Invalid value for `scope`. Must be one of: 'all', 'classes', 'value_sets', 'values'.",
      argcheck = check
    )
  if(!is.logical(suggest) || (!isTRUE(suggest) && !isFALSE(suggest)))
    addError(
      msg = "Invalid value for `suggest`.",
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
  if(length(scope) > 1){
    scope <- scope[1]
    addWarning(
      msg = "`scope` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }
  if(length(suggest) > 1){
    suggest <- suggest[1]
    addWarning(
      msg = "`suggest` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }
  if(length(output.mode) > 1){
    output.mode <- output.mode[1]
    addWarning(
      msg = "`output.mode` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }
  if(length(page) > 1){
    page <- page[1]
    addWarning(
      msg = "`page` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }
  if(length(page.size) > 1){
    page.size <- page.size[1]
    addWarning(
      msg = "`page.size` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/search",
    query = list(
      q = query,
      scope = scope,
      sources = sources,
      suggest = suggest,
      page = page,
      page_size = page.size
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}


