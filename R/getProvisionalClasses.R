#' Search values in CEDAR
#'
#' Get all provisional classes in a particular ontology (including provisional
#' value sets and provisional values). N.B. Since provisional classes are less
#' stable than regular ones, the choice was made to write a separate method for
#' those ones.
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
#' @return
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
#' result <- cedarr::accessOntology(
#'   my.api.key,
#'   "ENVO"
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessOntology <- function(
  api.key,
  ontology,
  output.mode = "content",
  page = 1,
  page.size = 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(ontology))
    stop("No ontology ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(ontology) || !is.na(ontology))
    addError(
      msg = "Invalid type for `ontology`.",
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
  if(is.na(ontology))
    sub <- NULL
  else if(is.character(ontology))
    ontology <- paste0("/", ontology)
  sapply(c("output.mode", "page","page.size"), function(arg){
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
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies",
      ontology,
      "/classes/provisional",
    ),
    query = list(
      page = page,
      page_size = page.size
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}


