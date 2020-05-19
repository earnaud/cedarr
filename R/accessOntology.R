#' Search values in CEDAR
#'
#' Get all classes from a specific ontology (including both regular and provisional classes).
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display. In this context, ontology can be set to NA:
#' this will list all the ontologies registered in CEDAR.
#' @param sub character. What sub-items shall be retrieved: none, its classes or its
#' properties? (resp. NA, "classes" or "properties")
#' @param sub.level character. At which level `sub` shall be fetched: all or only root?
#' (resp. NA or "roots"). This parameter will only be evaluated if `sub` is filled.
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#' @param page integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults to 50).
#'
#' @example
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
  sub = NA_character_,
  sub.level = NA_character_,
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
  if(!is.character(ontology) || !is.na(ontology) || grepl("^ontologies", ontology))
    addError(
      msg = "Invalid type for `ontology`. (string starting by \"ontologies\" are a reserved
      term)",
      argcheck = check
    )
  else if(!is.character(sub) ||
      !sub %in% c(NA, NA_character_, "classes", "properties"))
    addError(
      msg = "Invalid value for `sub`.",
      argcheck = check
    )
  else if(!is.character(sub.level) ||
      !sub.level %in% c(NA, NA_character_, "roots"))
    addError(
      msg = "Invalid value for `sub.level`.",
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
    if(is.na(sub))
      ontology <- paste0("/", ontology)
    else if(sub == "classes") {
      if(is.na(sub.level))
        ontology <- paste0(ontology, "/classes")
      else if(sub.level == "root")
        ontology <- paste0(ontology, "/classes/roots")
    }
    else if(sub == "properties") {
      if(is.na(sub.level))
        ontology <- paste0(ontology, "/properties")
      else if(sub.level == "root")
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
  result <- cedar.get(
    api.key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology
    ),
    query = ifelse(is.null(ontology),
      list(
        page = page,
        page_size = page.size
      ),
      NULL
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
