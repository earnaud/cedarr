#' Search values in CEDAR
#'
#' Find all value set collections.
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param vs.collection character. A VS collection id.
#' @param id character. A value ID in the VS collection. Not providing
#' (set NA) an id will list all values in the `vs.collection`.
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
#' @param page integer. Index of the page to be returned (defaults to 1st page).
#' @param page.size integer. Number of results per page, capped at 50. (defaults
#' to 50).
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
#' result <- cedarr::accessValueSets(
#'   my.api.key,
#'   vs.collection = "CEDARVS",
#'   id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type"
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
#' @importFrom utils URLencode
accessValueSets <- function(
  api.key,
  vs.collection,
  id = NA_character_,
  sub = NA_character_,
  output.mode = "content",
  page = 1,
  page.size= 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(vs.collection))
    stop("No VS collection ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(vs.collection) || is.na(vs.collection))
    addError(
      msg = "Invalid VS collection name: must be a length-one character.",
      argcheck = check
    )
  if(!is.character(id) &&
      !is.na(id))
    addError(
      msg = "Invalid value for `id`: must be either NA or a character.",
      argcheck = check
    )
  else if(isFALSE(is.character(sub) || is.na(sub)) &&
      !sub %in% c(NA, NA_character_, "tree", "values"))
    addError(
      msg = "Invalid value for `sub`: must be either NA or a character.",
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
    else if(sub == "values")
      id <- paste0("/", id, "/values")
  }
  sapply(c("vs.collection", "id", "sub", "output.mode", "page","page.size"), function(arg){
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
  result <- ifelse(
    is.null(id) || (!is.null(id) && sub == "values"),
    cedar.get(
      api.key,
      paste0(
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/value-sets",
        id
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
        "https://terminology.metadatacenter.org/bioportal/vs-collections/",
        vs.collection,
        "/value-sets",
        id
      ),
      output.mode = output.mode
    )
  )

  # Output ====
  return(result)
}
