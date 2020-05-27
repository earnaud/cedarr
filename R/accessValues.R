#' Explore values in CEDAR
#'
#' Find value by id.
#'
#' @param api.key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param vs.collection character. A VS collection id.
#' @param id character. A value ID in a value sets from the VS collection.
#' @param sub character. A sub-item to fetch from the value. Not
#' evaluated if `id` is set to NA. Else, can be NA, "value-set", "tree"
#' or "all-values". "value-set" allows to get the value-set the value
#' belongs to.
#' @param output.mode character. "full" will return the whole
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
#' result <- cedarr::accessValues(
#'   my.api.key,
#'   vs.collection = "CEDARVS",
#'   id = ??? # WIP
#' )
#'
#' View(result)
#'
#' @export
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
accessValues <- function(
  api.key,
  vs.collection,
  id,
  sub = NA_character_,
  output.mode = "content",
  page.index = 1,
  page.size= 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(vs.collection))
    stop("No VS collection ID provided.")
  if(missing(id))
    stop("No value ID provided.")

  # Invalid ====
  check <- newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode", "page.index", "page.size"),
    check = check, env = environment()
  )

  if(!is.character(vs.collection) || is.na(vs.collection))
    addError(
      msg = "Invalid VS collection name: must be a length-one character.",
      argcheck = check
    )
  if(!is.character(id) || is.na(id))
    addError(
      msg = "Invalid value for `id`: must be either a length-one character.",
      argcheck = check
    )
  else if(isFALSE(is.character(sub) || is.na(sub)) &&
      !sub %in% c(NA, NA_character_,"value-set", "tree", "all-values"))
    addError(
      msg = "Invalid value for `sub`: must be either NA or a character.",
      argcheck = check
    )

  # Correction ====
  check <- checkLength(
    c("api.key", "vs.collection", "id", "sub", "output.mode", "page.index","page.size"),
    check = check, env = environment()
  )

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

  finishArgCheck(check)

  # Request ====
  result <- ifelse(
    is.nulll(id) || (!is.null(id) && sub == "all-values"),
    cedar.get(
      api.key,
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
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/values",
        id
      ),
      output.mode = output.mode
    )
  )

  # Output ====
  return(result)
}
