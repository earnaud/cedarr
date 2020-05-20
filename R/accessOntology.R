#' Search values in CEDAR
#'
#' Get all classes from a specific ontology (including both regular and provisional classes).
#'
#' @param api.key  character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display. In this context, ontology
#' can be set to NA: this will list all the ontologies registered in CEDAR.
#' @param item character. What ontology items shall be retrieved: none, its classes or its
#' properties? (resp. NA, "classes" or "properties")
#' @param sub character. At which level `item` shall be fetched: all or only root?
#' (resp. NA or "roots"). This parameter will only be evaluated if `item` is filled.
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#' @param page integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults to 50).
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
  ontology = NA_character_,
  item = NA_character_,
  sub = NA_character_,
  output.mode = "content",
  page = 1,
  page.size = 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(ontology) || !is.na(ontology) || grepl("^ontologies", ontology))
    addError(
      msg = "Invalid type for `ontology`. (string starting by \"ontologies\" are a reserved
      term)",
      argcheck = check
    )
  else if(!is.character(item) ||
      !item %in% c(NA, NA_character_, "classes", "properties"))
    addError(
      msg = "Invalid value for `item`.",
      argcheck = check
    )
  else if(!is.character(sub) ||
      !sub %in% c(NA, NA_character_, "roots"))
    addError(
      msg = "Invalid value for `sub`.",
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
    ontology <- NULL
  else {
    if(is.na(item))
      ontology <- paste0("/", ontology)
    else if(item == "classes") {
      if(is.na(sub))
        ontology <- paste0(ontology, "/classes")
      else if(sub == "root")
        ontology <- paste0(ontology, "/classes/roots")
    }
    else if(item == "properties") {
      if(is.na(sub))
        ontology <- paste0(ontology, "/properties")
      else if(sub == "root")
        ontology <- paste0(ontology, "/properties/roots")
    }
  }
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
  result <- ifelse(is.null(ontology),
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/ontologies/",
        ontology
      ),
      query = list(
        page = page,
        page_size = page.size
      ),
      output.mode = output.mode
    ),
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/ontologies/",
        ontology
      ),
      output.mode = output.mode
    )
  )

  # Output ====
  return(result)
}
