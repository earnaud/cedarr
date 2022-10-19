#' CEDAR HTTP get method
#'
#' Internal function wrapping the GET method.
#'
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile).
#' @param url character. An URL to the CEDAR resource to query data from.
#' @param ... arguments to input in the request body.
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
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
#' For examples, see access*() functions from this package.
#'
#' @importFrom dplyr %>%
#' @importFrom httr GET add_headers
#' @importFrom jsonlite validate fromJSON
#' @importFrom checkmate assert anyMissing checkString
cedar.get <- function(api.key, url, ..., output.mode = "content"){
  assert(combine = "and",
    # Missing ====
    !anyMissing(c(api.key, url)),
    # Invalid
    checkString(api.key, pattern = "^apiKey")
    # ... shall already be checked before
  )

  message(sprintf("* Request URL: %s", url))

  result <- httr::GET(
    url,
    ...,
    httr::add_headers(Authorization = api.key)
  )

  if(is.raw(result$content))
    result$content <- result$content %>% rawToChar()
  if(jsonlite::validate(result$content))
    result$content <- result$content %>% jsonlite::fromJSON()

  message("* Request status:", result$status_code)

  # Output ====
  if(result$status_code != "200")
    stop(sprintf("Query returned code %s", result$status_code))
  else if(output.mode == "full")
    return(result)
  else
    return(result$content)
}

#' Correct arguments length.
#'
#' @param ... list. A list of arguments to check for length.
#' @param check ArgCheck. An ArgCheck object from package {ArgumentCheck}.
#' @param env environment. The environment in which  ... is being evaluated.
#'
#' @details
#'
#' This function will perform side-effects actions, shortening excessively long
#' items to length-one items.
#'
#' @return
#'
#' The `check` argument.
#'
#' Check for each given argument in `...` that their length > 1.
#'
# checkLength <- function(..., check, env = .GlobalEnv){
#   args <- as.list(...)
#
#   sapply(args, function(arg, .env = env){
#     arg.value <- get(arg, envir = .env)
#     if(length(arg.value) > 1){
#       assign(arg, arg.value, envir = .env)
#       ArgumentCheck::addWarning(
#         msg= paste0("`",arg,"` argument had length>1: only the first element is used."),
#         argcheck = check
#       )
#     }
#   })
#
#   return(check)
# }

#' These checks are found in every function of {cedarr}.
#'
# constantCheck <- function(..., check, env = .GlobalEnv){
#   args <- as.list(...)
#
#   sapply(args, function(arg, .env = env){
#     arg.value <- get(arg, envir = .env)
#
#     # Check type
#     assert(switch(arg,
#       api.key = checkClass(arg.value, "character"),
#       output.mode = checkClass(arg.value, "character"),
#       page.index = checkClass(arg.value, "numeric"),
#       page.size = checkClass(arg.value, "numeric"),
#     ))
#
#     # Check value
#     if(switch(arg,
#       output.mode = !arg.value %in% c("full", "content"),
#       page.index = as.integer(arg.value) == 0,
#       page.size = as.integer(arg.value) == 0,
#       FALSE
#     ))
#       ArgumentCheck::addError(
#         msg = paste("Invalid value for", arg),
#         argcheck = check
#       )
#
#   })
#
#   return(check)
# }

#' check API key
#' @noRd
checkApiKey <- function(api.key) {
  if(!grepl("^apiKey", api.key))
    api.key <- sprintf("apiKey %s", api.key)

  return(api.key)
}
