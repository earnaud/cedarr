#' Access Value Sets
#'
#' Get access to all or part of the value sets by VS collection.
#'
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param vs.collection character. A VS collection id.
#' @param id character. A value ID in the VS collection at URL format.
#' Not providing it (setting it to NA) will list all value sets in the `vs.collection`.
#' @param sub character. A sub-item to fetch from the value: can be NA, "tree",
#' or "values". Not evaluated if `id` is set to NA.
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to "content")
#' @param page.index integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults
#' to 50).
#'
#' @details
#'
#' This function matches the following queries from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`/vs-collection/{vs_collection}/value-sets`}
#'   \item{`/vs-collection/{vs_collection}/value-sets/{id}`}
#'   \item{`/vs-collection/{vs_collection}/value-sets/{id}/tree`}
#'   \item{`/vs-collection/{vs_collection}/value-sets/{id}/values`}
#' }
#'
#' These differents requests are differenciated by the `id` and `sub`
#' arguments.
#'
#' @return
#'
#' \itemize{
#'   \item{If only a VS collection ID is provided: a list of the value sets in
#'   this VS collection.}
#'   \item{If a VS collection ID and a value set ID are provided: an entry for
#'   the corresponding value set. With `sub` = "tree", retrieve the root values.
#'   If `sub` = "values", retrieve the value set's values.}
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
#' \dontrun{
#' my.api.key <- readline()
#'
#' result <- cedarr::accessValueSets(
#'   my.api.key,
#'   vs.collection = "CEDARVS",
#'   id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type"
#' )
#'
#' View(result)
#' }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkNumber checkString
accessValueSets <- function(
  api.key,
  vs.collection,
  id = NA_character_,
  sub = NA_character_,
  output.mode = "content",
  page.index = 1,
  page.size= 50
){

  assert(combine = "and",
    # Missing ====
    !anyMissing(c(api.key, vs.collection)),
    # Invalid ====
    checkString(api.key, pattern = "^apiKey"),
    checkChoice(output.mode, c("full", "content")),
    checkCharacter(vs.collection),
    checkCharacter(id),
    checkChoice(sub, c(NA, NA_character_, "tree", "values", "value")),
    checkNumber(page.index),
    checkNumber(page.size)
  )

  # Correction ====
  if(is.na(id)){
    id <- NULL
    sub <- NULL
  }
  else {
    id <- URLencode(id, reserved = TRUE)
    if(is.na(sub))
      id <- paste0("/", id)
    else if(sub == "tree")
      id <- paste0("/", id, "/tree")
    else if(grepl("valu", sub))
      id <- paste0("/", id, "/values")
    else
      id <- NULL
  }

  # Request ====
  result <- if(is.null(id) || isTRUE(!is.null(id) && sub == "values"))
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/value-sets",
        id
      ),
      query = list(
        page = as.integer(page.index),
        page_size = as.integer(page.size)
      ),
      output.mode = output.mode
    )
  else
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/value-sets",
        id
      ),
      output.mode = output.mode
    )

  # Output ====
  return(result)
}
