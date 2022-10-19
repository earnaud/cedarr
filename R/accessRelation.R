#' Search relation in CEDAR
#'
#' Find provisional relation by id. DISCLAIMER: at current development stage of
#' the API, this function is not operational (empty content) !
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It
#' is used within {cedarr} as a header for http requests. An API key is linked
#' to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param id character. Provisional relation short identifier.
#' @param output.mode character. "full" will return the whole response object
#' (from {httr}) or "content" will fetch the interest values from the response
#' object. Getting the whole object might be interesting to have a look at system
#' metadata, or in case of error to debug the connection. (defaults to "content")
#'
#' @details
#'
#' DISCLAIMER: at current development stage of the API, this function is not
#' operational !
#'
#' This function matches the following queries from the Swagger UI
#' (https://terminology.metadatacenter.org/api/#/):
#'
#' \itemize{
#'   \item{`/relations/{id}`}
#' }
#'
#' @return
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned,
#' containing database metadata and the interesting information in the
#' `collection` subitem.
#'
# @examples
# \dontrun{
# my.api.key <- readline()
#
# result <- cedarr::accessRelation(
#   my.api.key,
#   id = ???
# )
#
# View(result)
# }
#
#' @export
#' @importFrom checkmate assert anyMissing checkCharacter checkChoice checkString
accessRelation <- function(
  api.key,
  id,
  output.mode = "content"
){
  warning("DISCLAIMER: at current development stage of the package, this function
          concerns provisional terms !")

  assert(combine = "and",
    # Missing ====
    !anyMissing(c(api.key, id)),
    # Invalid ====
    checkString(api.key, pattern = "^apiKey"),
    checkChoice(output.mode, c("full", "content")),
    checkCharacter(id)
  )

  # Request ====
  id <- URLencode(id)
  result <- cedar.get(
    api.key,
    paste0("https://terminology.metadatacenter.org/bioportal/relations/", id),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
