#' Access Classes
#'
#' Access classes (including both regular and provisional) by ontology and class
#' id.
#'
#' @param api_key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display.
#' @param id character. Class ID to get, formatted as an URL.
#' (item `@id` in the result of accessOntology())
#' @param sub character. Class content ID to retrieve. Can be:
#' NA, "tree", "children", "descendants" or "parents".
#' @param output_mode character. "full" will return the whole
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
#'   \item{`/ontologies/{ontology}/classes/{id}`}
#'   \item{`/ontologies/{ontology}/classes/{id}/tree`}
#'   \item{`/ontologies/{ontology}/classes/{id}/children`}
#'   \item{`/ontologies/{ontology}/classes/{id}/descendants`}
#'   \item{`/ontologies/{ontology}/classes/{id}/parents`}
#' }
#'
#' These differents requests are differenciated by the `sub` argument.
#'
#' @return
#'
#' Setting `sub` to NA will retrieve the class item such as it is presented in
#' the return of `accessOntology()` (item `@id`).
#' Setting `sub` to "tree" will return the roots of the ontology in a data.frame.
#' Setting `sub` to "children" will return all of the descendants from the class
#' that happen to be one level below the class itself in its ontology.
#' Setting `sub` to "descendants" will return all of the descendants from the
#' class, whatever is their level below the target class.
#' Setting `sub` to "parents" will return the class coming before the target
#' class in its ontology.
#'
#' If `output_mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output_mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @examples
#' \dontrun{
#' my_api_key <- readline()
#'
#' # Fetch 'biome' class in ENVO
#' result <- cedarr::accessClass(
#'   my_api_key,
#'   "ENVO",
#'   "http://purl.obolibrary.org/obo/ENVO_00000428"
#' )
#'
#' View(result)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkString checkChoice
#' @importFrom utils URLencode
accessClass <- function(
  api_key,
  ontology,
  id,
  sub = NA_character_,
  output_mode = "content"
){
  assert(combine = "and",
    # Missing ====
    !anyMissing(c(api_key, ontology, id)),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content")),
    checkCharacter(ontology),
    checkCharacter(id),
    checkChoice(sub, c(NA, NA_character_, "tree", "children","descendants","parents"))
  )

  # Correction ====
  id <- utils::URLencode(id, reserved = TRUE)

  if(is.na(sub))
    sub <- NULL
  else if(is.character(sub) &&
      sub %in% c("tree", "children","descendants","parents")){
    sub <- paste0("/", sub)
  }

  # Request ====
  result <- cedarGet(
    api_key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology,
      "/classes/",
      id,
      sub
    ),
    output_mode = output_mode
  )

  # Output ====
  return(result)
}


