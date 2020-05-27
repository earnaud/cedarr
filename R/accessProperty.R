#' Access Properties
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
#' according to this value. Not evaluated if `id` is set to NA or "roots".
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#'
#' @details
#'
#' This function matches the following queries from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`/ontologies/{ontology}/properties`}
#'   \item{`/ontologies/{ontology}/properties/roots`}
#'   \item{`/ontologies/{ontology}/properties/{id}`}
#'   \item{`/ontologies/{ontology}/properties/{id}/tree`}
#'   \item{`/ontologies/{ontology}/properties/{id}/children`}
#'   \item{`/ontologies/{ontology}/properties/{id}/descendants`}
#'   \item{`/ontologies/{ontology}/properties/{id}/parents`}
#' }
#'
#' These differents requests are differenciated by the `id` and `sub`
#' arguments.
#'
#' @return
#'
#' \itemize{
#'   \item{If no property ID is provided: list all of the properties, or all the
#'   ontology's root properties if `sub` = "roots".}
#'   \item{If a property ID is provided: an entry for the corresponding property,
#'   or the roots properties if `sub` = "tree", the properties defined at the
#'   level under the target property if `sub` = "children", the lowest properties
#'   defined under the target property if `sub` = "descendants" or the properties
#'   defined previously to the one targeted if `sub` = "parents".}
#' }
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
#' # Query 1: list the roots properties in ENVO
#'
#' result1 <- cedarr::accessProperty(
#'   my.api.key,
#'   "ENVO",
#'   id = "roots",
#'   sub = "smurf" # ignored
#' )
#'
#' View(result1)
#'
#' # Query 2: get the parents properties for "alternative term" in ENVO
#'
#' result2 <- cedarr::accessProperty(
#'   my.api.key,
#'   "ENVO",
#'   id = "http://purl.obolibrary.org/obo/IAO_0000118",
#'   sub = "parents"
#' )
#'
#' View(result2)
#'
#' @export
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
#' @importFrom utils URLencode
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

  check <- constantCheck(
    c("api.key", "output.mode"),
    check = check, env = environment()
  )

  if(!is.character(ontology) || is.na(ontology))
    addError(
      msg = "Invalid type for `ontology`.",
      argcheck = check
    )
  if(isFALSE(is.character(id) || is.na(id)))
    addError(
      msg = "Invalid type for `id`.",
      argcheck = check
    )
  else if(isFALSE(
    id %in% c("root", "roots") ||
      grepl("^http", id) ||
      is.na(id)
  ))
    addError(
      msg = "Invalid value for `id`.",
      argcheck = check
    )
  else if(is.character(id) &&
      id != "roots"){
    if(isFALSE(is.character(sub) || is.na(sub)))
      addError(
        msg = "Invalid type for `sub`.",
        argcheck = check
      )
    else if(!sub %in% c(NA, NA_character_, "tree", "child", "children",
      "descendant", "descendants", "parent", "parents"))
      addError(
        msg = "Invalid value for `sub`.",
        argcheck = check
      )
  }

  # Correction ====
  check <- checkLength(
    c("api.key", "ontology", "output.mode", "id", "sub"),
    check = check, env = environment()
  )

  if(is.na(id))
    id <- NULL
  else if(id %in% c("root", "roots"))
    id <- "/roots"
  else {
    id <- URLencode(id, reserved = TRUE)
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
    output.mode = output.mode
  )

  # Output ====
  return(result)
}


