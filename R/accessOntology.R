#' Access Ontologies
#'
#' Access ontologies by id and list their content.
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
#' # Find the root classes of ENVO
#' result <- cedarr::accessOntology(
#'   my.api.key,
#'   "ENVO",
#'   item = "classes",
#'   sub = "roots"
#' )
#'
#' View(result)
#'
#' @export
#' @importFrom ArgumentCheck newArgCheck addError finishArgCheck
accessOntology <- function(
  api.key,
  ontology = NA_character_,
  item = NA_character_,
  sub = NA_character_,
  output.mode = "content",
  page.index = 1,
  page.size = 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- ArgumentCheck::newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode", "page.index", "page.size"),
    check = check, env = environment()
  )

  if(is.null(ontology)) ontology <- NA_character_
  if(isFALSE(is.character(ontology) || is.na(ontology)))
    if(grepl("^ontologies", ontology))
      ArgumentCheck::addError(
        msg = "Invalid type for `ontology`. (string starting by \"ontologies\" are a reserved
      term)",
        argcheck = check
      )
  else if(isFALSE(is.character(item) || is.na(item)) ||
          !item %in% c(
            NA, NA_character_,
            "class", "classe", "classes",
            "propert", "property", "properties"))
    ArgumentCheck::addError(
      msg = "Invalid value for `item`.",
      argcheck = check
    )
  else if(!is.character(sub) ||
          !sub %in% c(NA, NA_character_, "root", "roots"))
    ArgumentCheck::addError(
      msg = "Invalid value for `sub`.",
      argcheck = check
    )

  # Correction ====
  check <- checkLength(
    c("api.key", "ontology", "item", "sub", "output.mode", "page.index","page.size"),
    check = check, env = environment()
  )

  if(is.na(ontology))
    ontology <- NULL
  else {
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

  ArgumentCheck::finishArgCheck(check)

  # Request ====
  url <- paste0(
    "https://terminology.metadatacenter.org/bioportal/ontologies",
    ontology
  ) %>%
    gsub("//", "/", .)

  message(sprintf("* Request URL: %s", url))

  result <- if(is.null(ontology))
    cedar.get(
      api.key,
      url,
      query = list(
        page = page.index,
        page_size = page.size
      ),
      output.mode = output.mode
    )
  else
    cedar.get(
      api.key,
      url,
      output.mode = output.mode
    )

  # Output ====
  return(result)
}
