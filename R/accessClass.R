#' Search values in CEDAR
#'
#' Get all classes from a specific ontology (including both regular and provisional classes).
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display.
#' @param id character. Class ID to get, formatted as an URL (item `@id` in the result of
#' accessOntology()).
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#'
#' @example
#' my.api.key <- readline()
#'
#' result <- cedarr::accessClass(
#'   my.api.key,
#'   "ENVO"
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessClass <- function(
  api.key,
  ontology,
  id,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(ontology))
    stop("No ontology ID provided.")
  if(missing(class))
    stop("No class ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(ontology) || is.na(ontology))
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
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology,
      "/classes",
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


