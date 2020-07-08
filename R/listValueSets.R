#' List value sets and value sets collections
#'
#' Find all value sets or value sets collections.
#'
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#'
#' @return
#'
#' An exhaustive list of value sets for any value set collection.
#'
#' @examples
#' my.api.key <- readline()
#'
#' result1 <- cedarr::listValueSets(
#'   my.api.key
#' )
#'
#' View(result1)
#'
#' @export
#' @importFrom ArgumentCheck newArgCheck finishArgCheck
listValueSets <- function(
  api.key,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- ArgumentCheck::newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode"),
    check = check, env = environment()
  )

  # Correction ====
  check <- checkLength(
    "output.mode",
    check = check, env = environment()
  )

  ArgumentCheck::finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/value-sets",
    output.mode = output.mode
  )

  # Output ====
  return(result)
}

#' @describeIn listValueSets
#'
#' @return
#'
#' An exhaustive list of the VS collections in CEDAR.
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @examples
#'
#' result2 <- cedarr::listVSCollections(
#'   my.api.key,
#' )
#'
#' View(result2)
#'
#' @export
#' @importFrom ArgumentCheck newArgCheck addError finishArgCheck
listVSCollections <- function(
  api.key,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- ArgumentCheck::newArgCheck()

  check <- constantCheck(
    c("api.key", "output.mode"),
    check = check, env = environment()
  )

  # Correction ====
  check <- checkLength(
    "output.mode",
    check = check, env = environment()
  )

  ArgumentCheck::finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/vs-collections",
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
