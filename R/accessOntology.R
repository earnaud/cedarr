#' Access Ontologies
#'
#' Access ontologies by id and list their content.
#'
#' @param api_key  character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param ontology character. Ontology name to display. In this context, ontology
#' can be set to NA: this will list all the ontologies registered in CEDAR.
#' @param item character. What ontology items shall be retrieved: none, its classes or its
#' properties? (resp. NA, "classes" or "properties")
#' @param sub character. At which level `item` shall be fetched: all or only root?
#' (resp. NA or "roots"). This parameter will only be evaluated if `item` is filled.
#' @param output_mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#' @param page.index integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults to 50).
#'
#' @details
#'
#' This function matches the following queries from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`/ontologies`}
#'   \item{`/ontologies/{ontology}`}
#'   \item{`/ontologies/{ontology}/classes`}
#'   \item{`/ontologies/{ontology}/classes/roots`}
#'   \item{`/ontologies/{ontology}/properties`}
#'   \item{`/ontologies/{ontology}/properties/roots`}
#' }
#'
#' These differents requests are differenciated by the `item` and `sub`
#' arguments. The requests for `item = "properties"` are identical to some
#' available in `accessProperty()`.
#'
#' @return
#'
#'\itemize{
#'  \item{If no ontology ID is provided: returns a list of all ontologies registered in
#'  CEDAR.}
#'  \item{If an ontology ID is provided: returns an entry for this ontology.}
#'  \item{If an ontology ID is provided with `item` = "classes": returns either
#'  a list of the classes in this ontology, or at the root of this ontology if
#'  provided with `sub` = "roots".}
#'  \item{If an ontology ID is provided with `item` = "properties": returns either
#'  a list of the properties in this ontology, or at the root of this ontology if
#'  provided with `sub` = "roots".}
#'}
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
#' # Find the root classes of ENVO
#' result <- cedarr::accessOntology(
#'   my_api_key,
#'   "ENVO",
#'   item = "classes",
#'   sub = "roots"
#' )
#'
#' View(result)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkString testNull
accessOntology <- function(
  api_key,
  ontology = NA_character_,
  item = NA_character_,
  sub = NA_character_,
  output_mode = "content",
  page.index = 1,
  page.size = 50
){
  assert(combine = "and",
    # Missing ====
    !anyMissing(api_key),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content")),
    checkChoice(sub, c(NA, NA_character_, "root", "roots")),
    checkChoice(item, c(
      NA, NA_character_,
      "class", "classe", "classes",
      "propert", "property", "properties"
    ))
  )

  # Specific handling of ontology
  assert(combine = "or",
    checkString(ontology, na.ok = TRUE),
    testNull(ontology)
  )

  # Correction ====

  if(is.na(ontology))
    ontology <- NULL
  else if(!is.null(ontology)) {
    ontology <- paste0("/", ontology)
    if(grepl("class", item)) {
      if(is.na(sub))
        ontology <- paste0(ontology, "/classes")
      else if(grepl("root", sub))
        ontology <- paste0(ontology, "/classes/roots")
    }
    else if(grepl("propert", item)) {
      if(is.na(sub))
        ontology <- paste0(ontology, "/properties")
      else if(grepl("root", sub))
        ontology <- paste0(ontology, "/properties/roots")
    }
  }

  # Request ====
  url <- paste0(
    "https://terminology.metadatacenter.org/bioportal/ontologies",
    ontology
  ) %>%
    gsub(pattern = "//", replacement = "/")

  result <- if(is.null(ontology))
    cedarGet(
      api_key,
      url,
      query = list(
        page = page.index,
        page_size = page.size
      ),
      output_mode = output_mode
    )
  else
    cedarGet(
      api_key,
      url,
      output_mode = output_mode
    )

  # Output ====
  return(result)
}
