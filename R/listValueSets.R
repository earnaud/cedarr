#' Search values in CEDAR
#'
#' Find all value sets.
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#'
#' @example
#' my.api.key <- readline()
#'
#' result <- cedarr::listValueSets(
#'   my.api.key,
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
listValueSets <- function(
  api.key,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  if(length(output.mode) > 1){
    output.mode <- output.mode[1]
    addWarning(
      msg = "`output.mode` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/value-sets",
    output.mode = output.mode
  )

  # Output ====
  return(result)
}

#' Search values in CEDAR
#'
#' Find all value set collections.
#'
#  INTEREST ARGUMENTS
#' @param api.key character. An API Key is required to access any API call. It is used within {cedarr}
#' as a header for http requests. An API key is linked to a CEDAR account (https://cedar.metadatacenter.org/profile)
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#'
#' @example
#' my.api.key <- readline()
#'
#' result <- cedarr::listVSCollections(
#'   my.api.key,
#' )
#'
#' View(result)
#'
#' @importFrom ArgumentCheck newArgCheck finishArgCheck addError addWarning
listVSCollections <- function(
  api.key,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  if(length(output.mode) > 1){
    output.mode <- output.mode[1]
    addWarning(
      msg = "`output.mode` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    "https://terminology.metadatacenter.org/bioportal/vs-collections",
    output.mode = output.mode
  )

  # Output ====
  return(result)
}
