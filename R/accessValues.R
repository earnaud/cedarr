#' Explore values in CEDAR
#'
#' Find value by id.
#'
#' @param api_key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param vs.collection character. A VS collection id.
#' @param id character. A value ID in a value sets from the VS collection.
#' @param sub character. A sub-item to fetch from the value. Not
#' evaluated if `id` is set to NA. Else, can be NA, "value-set", "tree"
#' or "all-values". "value-set" allows to get the value-set the value
#' belongs to.
#' @param output_mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
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
#'   \item{`/vs-collections/{vs_collection}/values/{id}`}
#'   \item{`/vs-collections/{vs_collection}/values/{id}/tree`}
#'   \item{`/vs-collections/{vs_collection}/values/{id}/all-values`}
#'   \item{`/vs-collections/{vs_collection}/values/{id}/value-set`}
#' }
#'
#' These differents requests are differenciated by the `id` and `sub`
#' arguments.
#'
#' @return
#'
#' \itemize{
#'   \item{If a single value ID is provided: an entry for the corresponding value.}
#'   \item{If a value ID is provided and `sub` = "tree": get value tree (only for
#'    regular values).}
#'   \item{If a value ID is provided and `sub` = "all-values": find all values
#'   in the value set that the given value belongs to.}
#'   \item{If a value ID is provided and `sub` = "value-set": an entry for the
#'   value set the target values belong to.}
#' }
#'
#' If `output_mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output_mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
# @examples
# \dontrun{
# my_api_key <- readline()
#
# result <- cedarr::accessValues(
#   my_api_key,
#   vs.collection = "CEDARVS",
#   id = ??? # WIP
# )
#
# View(result)
# }
#'
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkNumber checkString
accessValues <- function(
  api_key,
  vs.collection,
  id,
  sub = NA_character_,
  output_mode = "content",
  page.index = 1,
  page.size= 50
){
  warning("DISCLAIMER: at current development stage of the package, this function is not
operational !", immediate. = TRUE)

  assert(combine = "and",
    # Missing ====
    !anyMissing(c(api_key, vs.collection, id)),
    # Invalid ====
    checkString(api_key, pattern = "^apiKey"),
    checkChoice(output_mode, c("full", "content")),
    checkNumber(page.index),
    checkNumber(page.size),
    checkChoice(sub, c(NA, NA_character_,"value-set", "tree", "all-values"))
  )

  # Correction ====
  if(is.na(id)){
    id <- NULL
    sub <- NULL
  }
  else {
    if(is.na(sub))
      id <- paste0("/", id)
    else if(sub == "value-set")
      id <- paste0("/", id, "/value-set")
    else if(sub == "tree")
      id <- paste0("/", id, "/tree")
    else if(sub == "all-values")
      id <- paste0("/", id, "/all-values")
  }

  # Request ====
  browser()
  result <- ifelse(
    is.null(id) || (!is.null(id) && identical(sub, "all-values")),
    cedarGet(
      api_key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/values",
        id
      ),
      query = list(
        page = page.index,
        page_size = page.size
      )
    ),
    cedarGet(
      api_key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/values",
        id
      ),
      output_mode = output_mode
    )
  )

  # Output ====
  return(result)
}
