#' Access Provisional Classes
#'
#' Get all provisional classes in a particular ontology (including provisional
#' value sets and provisional values).
#'
#' @param api.key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display. In this context, ontology
#' can be set to NA: this will list all provisional classes from CEDAR.
#' @param output.mode character. "full" will return the whole response object
#' (from {httr}) or "content" will fetch the interest values from the response
#' object. Getting the whole object might be interesting to have a look at system
#' metadata, or in case of error to debug the connection. (defaults to "content")
#' @param page integer. Index of the page to be returned (defaults to 1st page).
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
#' result <- cedarr::accessProvisional(
#'   my.api.key,
#'   "ENVO"
#' )
#'
#' View(result)
#'
#' @export
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessProvisional <- function(
  api.key,
  ontology,
  output.mode = "content",
  page.index = 1,
  page.size = 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(ontology))
    stop("No ontology ID provided.")

  # Invalid ====
  check <- newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode", "page.index", "page.size"),
    check = check, env = environment()
  )

  if(isFALSE(is.character(ontology) || is.na(ontology)))
    addError(
      msg = "Invalid type for `ontology`.",
      argcheck = check
    )

  # Correction ====
  check <- checkLength(
    c("api.key", "output.mode", "page.index","page.size"),
    check = check, env = environment()
  )

  if(is.na(ontology))
    sub <- NULL
  else if(is.character(ontology))
    ontology <- paste0("/", ontology)

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies",
      ontology,
      "/classes/provisional"
    ),
    query = list(
      page = page.index,
      page_size = page.size
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}


