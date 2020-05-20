#' Access properties
#'
#' Function to access the CEDAR properties suite.
#'
#' @param api.key  character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Name of the ontology to query properties from.
#' @param id character. Either NA to list properties, "roots" to get the root
#' properties of the ontology or a property ID (at URL format), to find a
#' precise property.
#' @param sub character. Either NA to get the precise property alone, or one
#' among "tree", "children", "descendants", "parents" to get related properties
#' according to this value. Ignored if `id` is set to NA or "roots".
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
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
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessProperty <- function(
  api.key,
  ontology,
  id = NA_character_, # NA "roots" or ID
  sub = NA_character_, #
  output.mode = "content"
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
  if(!is.character(ontology) || is.na(ontology))
    addError(
      msg = "Invalid type for `ontology`. (string starting by \"ontologies\" are a reserved
      term)",
      argcheck = check
    )
  if(!is.character(id) && !is.na(id))
    addError(
      msg = "Invalid value for `sub`.",
      argcheck = check
    )
  else if(is.character(id) &&
      id != "roots" &&
      !sub %in% c(NA, NA_character_, "tree", "children", "descendants", "parents")) {
        addError(
          msg = "Invalid value for `sub`.",
          argcheck = check
        )
  }
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  sapply(c("api.key", "ontology", "output.mode", "id", "sub"), function(arg){
    if(length(get(arg)) > 1){
      assign(arg, get(arg)[1])
      addWarning(
        msg = "`",arg,"` argument had length > 1: only the first element is used.",
        argcheck = check
      )
    }
  })
  if(is.na(id))
    id <- NULL
  else if(id == "roots")
    id <- "/roots"
  else {
    if(is.na(sub))
      sub <- NULL
    id <- paste0(c("", id, sub), collapse = "/")
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology,
      "/properties",
      id
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


